package vee.db

import java.util

import cats.syntax.monoid._
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.state2._
import vee.transaction.ProcessedTransaction
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.ValidationError
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.Transaction

import scala.collection.JavaConverters._


trait Caches extends BlockChain {
  import Caches._

  protected def maxCacheSize: Int

  @volatile
  private var heightCache = loadHeight()
  protected def loadHeight(): Int
  override def height: Int = heightCache

  @volatile
  private var scoreCache = loadScore()
  protected def loadScore(): BigInt
  override def score: BigInt = scoreCache

  @volatile
  private var lastBlockCache = loadLastBlock()
  protected def loadLastBlock(): Option[Block]
  override def lastBlock: Option[Block] = lastBlockCache

  private val transactionIds                                       = new util.HashMap[ByteStr, Long]()
  protected def forgetTransaction(id: ByteStr): Unit               = transactionIds.remove(id)
  override def containsTransaction(id: ByteStr): Boolean           = transactionIds.containsKey(id)

  private val portfolioCache: LoadingCache[Address, Portfolio] = cache(maxCacheSize, loadPortfolio)
  protected def loadPortfolio(address: Address): Portfolio
  protected def discardPortfolio(address: Address): Unit = portfolioCache.invalidate(address)
  override def accountPortfolio(a: Address): Portfolio          = portfolioCache.get(a)
  override def accountPortfolios: Map[Address, Portfolio]       = portfolioCache.asMap().asScala.toMap

  private var lastAddressId = loadMaxAddressId()
  protected def loadMaxAddressId(): BigInt

  private val addressIdCache: LoadingCache[Address, Option[BigInt]] = cache(maxCacheSize, loadAddressId)
  protected def loadAddressId(address: Address): Option[BigInt]
  protected def addressId(address: Address): Option[BigInt] = addressIdCache.get(address)

  protected def doAppend(block: Block,
                         //carryFee: Long,
                         addresses: Map[Address, BigInt],
                         veeBalances: Map[BigInt, Long],
                         leaseBalances: Map[BigInt, LeaseInfo],
                         leaseStates: Map[ByteStr, Boolean],
                         transactions: Map[ByteStr, (ProcessedTransaction, Set[BigInt])],
                         addressTransactions: Map[BigInt, List[ByteStr]]
                        ): Unit

  override def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] = {
    if ((heightCache == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
      heightCache += 1
      scoreCache += block.blockScore
      lastBlockCache = Some(block)

      val diff = blockDiff.txsDiff
      val newAddresses = Set.newBuilder[Address]
      newAddresses ++= diff.portfolios.keys.filter(addressIdCache.get(_).isEmpty)
      for ((_, _, addresses) <- diff.transactions.values; address <- addresses if addressIdCache.get(address).isEmpty) {
        newAddresses += address
      }

      val newAddressIds = (for {
        (address, offset) <- newAddresses.result().zipWithIndex
      } yield address -> (lastAddressId + offset + 1)).toMap

      def addressId(address: Address): BigInt = (newAddressIds.get(address) orElse addressIdCache.get(address)).get

      lastAddressId += newAddressIds.size

      val veeBalances = Map.newBuilder[BigInt, Long]
      val assetBalances = Map.newBuilder[BigInt, Map[ByteStr, Long]]
      val leaseBalances = Map.newBuilder[BigInt, LeaseInfo]
      val newPortfolios = Map.newBuilder[Address, Portfolio]

      for ((address, portfolioDiff) <- diff.portfolios) {
        val newPortfolio = portfolioCache.get(address).combine(portfolioDiff)
        if (portfolioDiff.balance != 0) {
          veeBalances += addressId(address) -> newPortfolio.balance
        }

        if (portfolioDiff.leaseInfo != LeaseInfo.empty) {
          leaseBalances += addressId(address) -> newPortfolio.leaseInfo
        }

        if (portfolioDiff.assets.nonEmpty) {
          val newAssetBalances = for {(k, v) <- portfolioDiff.assets if v != 0} yield k -> newPortfolio.assets(k)
          if (newAssetBalances.nonEmpty) {
            assetBalances += addressId(address) -> newAssetBalances
          }
        }

        newPortfolios += address -> newPortfolio
      }

      val newTransactions = Map.newBuilder[ByteStr, (ProcessedTransaction, Set[BigInt])]
      for ((id, (_, tx, addresses)) <- diff.transactions) {
        transactionIds.put(id, tx.transaction.timestamp)
        newTransactions += id -> ((tx, addresses.map(addressId)))
      }

      doAppend(
        block,
        newAddressIds,
        veeBalances.result(),
        leaseBalances.result(),
        diff.leaseState,
        newTransactions.result(),
        diff.accountTransactionIds.map({ case (addr, txs) => addressId(addr) -> txs })
      )

      for ((address, id) <- newAddressIds) addressIdCache.put(address, Some(id))
      for ((address, portfolio) <- newPortfolios.result()) portfolioCache.put(address, portfolio)

      blockDiff
    } else {
      Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last local block ${this.lastBlock.map(_.uniqueId)}"))
    }
  }

  protected def doRollback(targetBlockId: ByteStr): Seq[Block]

  override def rollbackTo(targetBlockId: ByteStr): Seq[Block] = {
    val discardedBlocks = doRollback(targetBlockId)

    heightCache = loadHeight()
    scoreCache = loadScore()
    lastBlockCache = loadLastBlock()

    discardedBlocks
  }

  override def discardBlock(): Seq[Transaction] = loadLastBlock() match {
    case Some(lastBlock) =>
      rollbackTo(lastBlock.uniqueId)
      lastBlock.transactionData.map(_.transaction)
    case None => Seq.empty
  }
}

object Caches {
  def cache[K <: AnyRef, V <: AnyRef](maximumSize: Int, loader: K => V): LoadingCache[K, V] =
    CacheBuilder
      .newBuilder()
      .maximumSize(maximumSize)
      .build(new CacheLoader[K, V] {
        override def load(key: K) = loader(key)
      })
}
