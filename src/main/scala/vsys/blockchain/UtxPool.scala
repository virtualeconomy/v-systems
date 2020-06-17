package vsys.blockchain

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.ConcurrentHashMap

import cats._
import vsys.blockchain.UtxPool.PessimisticPortfolios
import vsys.settings.{FunctionalitySettings, UtxSettings}
import vsys.blockchain.state.diffs.TransactionDiffer
import vsys.blockchain.state.reader.{CompositeStateReader, StateReader}
import vsys.blockchain.state.{ByteStr, Diff, Portfolio}
import kamon.Kamon
import vsys.account.{Account, Address}
import vsys.blockchain.block.Block
import vsys.blockchain.history.History
import vsys.blockchain.consensus.TransactionsOrdering
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction._
import vsys.utils.{ScorexLogging, Synchronized, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Left, Right}

class UtxPool(time: Time,
              stateReader: StateReader,
              history: History,
              feeCalculator: FeeCalculator,
              fs: FunctionalitySettings,
              utxSettings: UtxSettings) extends Synchronized with ScorexLogging {

  def synchronizationToken: ReentrantReadWriteLock = new ReentrantReadWriteLock()

  private val transactions = new ConcurrentHashMap[ByteStr, Transaction]()
  private val pessimisticPortfolios = new PessimisticPortfolios

  private val sizeStats = Kamon.metrics.histogram("utx-pool-size")
  private val putRequestStats = Kamon.metrics.counter("utx-pool-put-if-new")

  private def removeExpired(currentTs: Long): Unit = write { implicit l =>
    def isExpired(tx: Transaction) = (currentTs - tx.timestamp).nanos > utxSettings.maxTransactionAge

    transactions.values.asScala.filter(isExpired).foreach {
      tx =>
        transactions.remove(tx.id)
        pessimisticPortfolios.remove(tx.id)
    }
  }

  def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = {
    putRequestStats.increment()
    for {
      _ <- Either.cond(transactions.size < utxSettings.maxSize, (), GenericError("Transaction pool size limit is reached"))
      _ <- Either.cond(tx.transactionType != TransactionType.MintingTransaction, (),
        GenericError("Cannot add MintingTransaction to transaction pool"))
      _ <- feeCalculator.enoughFee(tx)
      diff <- TransactionDiffer(fs, history.lastBlock.map(_.timestamp), time.correctedTime(), stateReader.height)(stateReader, tx)
    } yield {
      pessimisticPortfolios.add(tx.id, diff)
      sizeStats.record(transactions.size + 1)
      Option(transactions.put(tx.id, tx)).isEmpty
    }
  }

  def removeAll(tx: Traversable[Transaction]): Unit = write { implicit l =>
    removeExpired(time.correctedTime())
    tx.view.map(_.id).foreach { id =>
      transactions.remove(id)
      pessimisticPortfolios.remove(id)
    }
  }

  def portfolio(addr: Address): Portfolio = read { implicit l =>
    val base = stateReader.accountPortfolio(addr)
    val foundInUtx = pessimisticPortfolios.getAggregated(addr)

    Monoid.combine(base, foundInUtx)
  }

  def all(): Seq[Transaction] = read { implicit l =>
    transactions.values.asScala.toSeq.sorted(TransactionsOrdering.InUTXPool)
  }

  def size: Int = transactions.size

  def transactionById(transactionId: ByteStr): Option[Transaction] = Option(transactions.get(transactionId))

  def packUnconfirmed(): Seq[ProcessedTransaction] = write { implicit l =>
    val currentTs = time.correctedTime()
    removeExpired(currentTs)
    val differ = TransactionDiffer(fs, history.lastBlock.map(_.timestamp), currentTs, stateReader.height) _
    val (invalidTxs, validTxs, _) = transactions
      .values.asScala.toSeq
      .sorted(TransactionsOrdering.InUTXPool)
      .foldLeft((Seq.empty[ByteStr], Seq.empty[ProcessedTransaction], Monoid[Diff].empty)) {
        case ((invalid, valid, diff), tx) if valid.size < Block.MaxTransactionsPerBlockVer1 - 1 =>
          differ(new CompositeStateReader(stateReader, diff.asBlockDiff), tx) match {
            case Right(newDiff) =>
              (invalid, ProcessedTransaction(newDiff.txStatus, newDiff.chargedFee, tx) +: valid, Monoid.combine(diff, newDiff))
            case Left(e) =>
              log.debug(s"Removing invalid transaction ${tx.id} from UTX: $e")
              (tx.id +: invalid, valid, diff)
          }
        case (r, _) => r
      }

    invalidTxs.foreach { itx =>
      transactions.remove(itx)
      pessimisticPortfolios.remove(itx)
    }

    validTxs.reverse
  }
}

object UtxPool {

  private class PessimisticPortfolios {
    private type Portfolios = Map[Account, Portfolio]

    private val transactionPortfolios = new ConcurrentHashMap[ByteStr, Portfolios]()
    private val transactions = new ConcurrentHashMap[Account, Set[ByteStr]]()

    def add(txId: ByteStr, txDiff: Diff): Unit = {
      val nonEmptyPessimisticPortfolios = txDiff.portfolios
        .mapValues(_.pessimistic)
        .filterNot {
          case (_, portfolio) => portfolio.isEmpty
        }

      if (nonEmptyPessimisticPortfolios.nonEmpty &&
        Option(transactionPortfolios.put(txId, nonEmptyPessimisticPortfolios)).isEmpty) {
        nonEmptyPessimisticPortfolios.keys.foreach { address =>
          transactions.put(address, transactions.getOrDefault(address, Set.empty) + txId)
        }
      }
    }

    def getAggregated(accountAddr: Address): Portfolio = {
      val portfolios = for {
        txId <- transactions.getOrDefault(accountAddr, Set.empty).toSeq
        txPortfolios = transactionPortfolios.getOrDefault(txId, Map.empty[Account, Portfolio])
        txAccountPortfolio <- txPortfolios.get(accountAddr).toSeq
      } yield txAccountPortfolio

      Monoid.combineAll[Portfolio](portfolios)
    }

    def remove(txId: ByteStr): Unit = {
      if (Option(transactionPortfolios.remove(txId)).isDefined) {
        transactions.keySet().asScala.foreach { addr =>
          transactions.put(addr, transactions.getOrDefault(addr, Set.empty) - txId)
        }
      }
    }
  }

}
