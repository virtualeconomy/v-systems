package vsys.blockchain.state.reader

import vsys.account.Address
import vsys.blockchain.contract.{Contract, DataEntry}
import vsys.blockchain.state._
import vsys.blockchain.transaction._
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.lease.LeaseTransaction
import vsys.utils.{ScorexLogging, Synchronized}

import scala.annotation.tailrec
import scala.reflect.ClassTag

trait StateReader extends Synchronized {

  def accountPortfolios: Map[Address, Portfolio]

  def transactionInfo(id: ByteStr): Option[(Int, ProcessedTransaction)]

  def containsTransaction(id: ByteStr): Boolean

  def accountPortfolio(a: Address): Portfolio

  def height: Int

  def slotAddress(id: Int): Option[String]

  def addressSlot(add: String): Option[Int]

  def effectiveSlotAddressSize: Int

  def accountTransactionIds(a: Address, limit: Int, offset: Int): (Int, Seq[ByteStr])

  def txTypeAccountTxIds(txType: TransactionType.Value, a: Address, limit: Int, offset: Int): (Int, Seq[ByteStr])

  def accountTransactionsLengths(a: Address): Int

  def txTypeAccTxLengths(txType: TransactionType.Value, a: Address): Int

  def contractContent(id: ByteStr): Option[(Int, ByteStr, Contract)]

  def contractInfo(id: ByteStr): Option[DataEntry]

  def contractTokens(id: ByteStr): Int

  def tokenInfo(id: ByteStr): Option[DataEntry]

  def tokenAccountBalance(id: ByteStr): Long

  def dbGet(key: ByteStr): Option[ByteStr]

  def isLeaseActive(leaseTx: LeaseTransaction): Boolean

  def activeLeases(): Seq[ByteStr]

  def lastUpdateHeight(acc: Address): Option[Int]

  def lastUpdateWeightedBalance(acc: Address): Option[Long]

  def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot]
}

object StateReader {

  implicit class StateReaderExt(s: StateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteStr): Map[Address, Long] =
      s.accountPortfolios
        .mapValues(portfolio => portfolio.assets.get(assetId))
        .collect { case (acc, Some(amt)) => acc -> amt }

    def findTransaction[T <: Transaction](signature: ByteStr)(implicit ct: ClassTag[T]): Option[T]
    = s.transactionInfo(signature).map(_._2.transaction)
      .flatMap(tx => {
        if (ct.runtimeClass.isAssignableFrom(tx.getClass))
          Some(tx.asInstanceOf[T])
        else None
      })

    def included(signature: ByteStr): Option[Int] = s.transactionInfo(signature).map(_._1)

    def accountTransactions(account: Address, limit: Int, offset: Int): (Int, Seq[(Int, _ <: ProcessedTransaction)]) = s.read { _ =>
      val res = s.accountTransactionIds(account, limit, offset)
      (res._1, res._2.flatMap(s.transactionInfo))
    }

    def txTypeAccountTransactions(
      txType: TransactionType.Value,
      account: Address,
      limit: Int,
      offset: Int): (Int, Seq[(Int, _ <: ProcessedTransaction)]) = s.read { _ =>
      val res = s.txTypeAccountTxIds(txType, account, limit, offset)
      (res._1, res._2.flatMap(s.transactionInfo))
    }

    def balance(account: Address): Long = s.accountPortfolio(account).balance

    def effectiveBalance(account: Address): Long = s.accountPortfolio(account).effectiveBalance

    private def minBySnapshot(acc: Address, atHeight: Int, confirmations: Int)(extractor: Snapshot => Long): Long = s.read { _ =>
      val bottomNotIncluded = atHeight - confirmations

      @tailrec
      def loop(deeperHeight: Int, list: Seq[Snapshot]): Seq[Snapshot] = {
        if (deeperHeight == 0) {
          s.snapshotAtHeight(acc, 1) match {
            case Some(genesisSnapshot) =>
              genesisSnapshot +: list
            case None =>
              Snapshot(0, 0, 0, 0) +: list
          }
        } else {
          s.snapshotAtHeight(acc, deeperHeight) match {
            case Some(snapshot) =>
              if (deeperHeight <= bottomNotIncluded)
                snapshot +: list
              else if (snapshot.prevHeight == deeperHeight) {
                throw new Exception(s"CRITICAL: Infinite loop detected while calculating minBySnapshot: acc=$acc, atHeight=$atHeight, " +
                  s"confirmations=$confirmations; lastUpdateHeight=${s.lastUpdateHeight(acc)}; current step: deeperHeight=$deeperHeight, list.size=${list.size}")
              } else if (deeperHeight > atHeight && snapshot.prevHeight > atHeight) {
                loop(snapshot.prevHeight, list)
              } else
                loop(snapshot.prevHeight, snapshot +: list)
            case None =>
              throw new Exception(s"CRITICAL: Cannot lookup referenced height: acc=$acc, atHeight=$atHeight, " +
                s"confirmations=$confirmations; lastUpdateHeight=${s.lastUpdateHeight(acc)}; current step: deeperHeight=$deeperHeight, list.size=${list.size}")
          }
        }
      }

      val snapshots: Seq[Snapshot] = s.lastUpdateHeight(acc) match {
        case None => Seq(Snapshot(0, 0, 0, 0))
        case Some(h) if h < bottomNotIncluded =>
          val pf = s.accountPortfolio(acc)
          Seq(Snapshot(h, pf.balance, pf.effectiveBalance, s.lastUpdateWeightedBalance(acc).getOrElse(0)))
        case Some(h) => loop(h, Seq.empty)
      }

      snapshots.map(extractor).min
    }

    def effectiveBalanceAtHeightWithConfirmations(acc: Address, atHeight: Int, confirmations: Int): Long =
      minBySnapshot(acc, atHeight, confirmations)(_.effectiveBalance)

    def balanceWithConfirmations(acc: Address, confirmations: Int): Long =
      minBySnapshot(acc, s.height, confirmations)(_.balance)

    def weightedBalanceWithConfirmations(acc: Address, confirmations: Int): Long =
      minBySnapshot(acc, s.height, confirmations)(_.weightedBalance)

    def balanceAtHeight(acc: Address, height: Int): Long = s.read { _ =>

      @tailrec
      def loop(lookupHeight: Int): Long = s.snapshotAtHeight(acc, lookupHeight) match {
        case None if lookupHeight == 0 => 0
        case Some(snapshot) if lookupHeight <= height => snapshot.balance
        case Some(snapshot) => loop(snapshot.prevHeight)
        case None =>
          throw new Exception(s"Cannot lookup account $acc for height $height(current=${s.height}). " +
            s"No history found at requested lookupHeight=$lookupHeight")
      }

      loop(s.lastUpdateHeight(acc).getOrElse(0))
    }

    def weightedBalanceAtHeight(acc: Address, height: Int): Long = s.read { _ =>

      @tailrec
      def loop(lookupHeight: Int): Long = s.snapshotAtHeight(acc, lookupHeight) match {
        case None if lookupHeight == 0 => 0
        case Some(snapshot) if lookupHeight <= height => snapshot.weightedBalance
        case Some(snapshot) => loop(snapshot.prevHeight)
        case None =>
          throw new Exception(s"Cannot lookup account $acc for height $height(current=${s.height}). " +
            s"No history found at requested lookupHeight=$lookupHeight")
      }

      loop(s.lastUpdateHeight(acc).getOrElse(0))
    }

    def accountPortfoliosHash: Int = {
      Hash.accountPortfolios(s.accountPortfolios)
    }
  }
}
