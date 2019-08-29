package vsys.blockchain.state

import cats.Monoid
import cats.implicits._
import vsys.account.{Account, Address}
import vsys.blockchain.transaction.Transaction
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.database.Entry
import vsys.blockchain.contract.Contract
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus}

case class Snapshot(prevHeight: Int, balance: Long, effectiveBalance: Long, weightedBalance: Long)

case class LeaseInfo(leaseIn: Long, leaseOut: Long)

object LeaseInfo {
  val empty = LeaseInfo(0, 0)
  implicit val leaseInfoMonoid = new Monoid[LeaseInfo] {
    override def empty: LeaseInfo = LeaseInfo.empty

    override def combine(x: LeaseInfo, y: LeaseInfo): LeaseInfo = LeaseInfo(safeSum(x.leaseIn, y.leaseIn), safeSum(x.leaseOut, y.leaseOut))
  }
}

case class OrderFillInfo(volume: Long, fee: Long)

object OrderFillInfo {
  implicit val orderFillInfoMonoid = new Monoid[OrderFillInfo] {
    override def empty: OrderFillInfo = OrderFillInfo(0, 0)

    override def combine(x: OrderFillInfo, y: OrderFillInfo): OrderFillInfo = OrderFillInfo(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AssetInfo(isReissuable: Boolean, volume: Long)

object AssetInfo {
  implicit val assetInfoMonoid = new Monoid[AssetInfo] {
    override def empty: AssetInfo = AssetInfo(isReissuable = true, 0)

    override def combine(x: AssetInfo, y: AssetInfo): AssetInfo
    = AssetInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}

case class Diff(transactions: Map[ByteStr, (Int, ProcessedTransaction, Set[Account])],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteStr, AssetInfo],
                slotids: Map[Int, Option[String]],
                addToSlot: Map[String, Option[Int]],
                slotNum: Int,
                txStatus: TransactionStatus.Value,
                chargedFee: Long,
                contracts: Map[ByteStr, (Int, ByteStr, Contract, Set[Address])],
                contractDB: Map[ByteStr, Array[Byte]],
                contractTokens: Map[ByteStr, Int],
                tokenDB: Map[ByteStr, Array[Byte]],
                tokenAccountBalance: Map[ByteStr, Long],
                dbEntries: Map[ByteStr, Entry],
                orderFills: Map[ByteStr, OrderFillInfo],
                leaseState: Map[ByteStr, Boolean]) {

  lazy val accountTransactionIds: Map[Account, List[ByteStr]] = {
    val map: List[(Account, Set[(Int, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.transaction.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Account, Set[(Int, Long, ByteStr)]]) { case (m, (acc, set)) =>
      m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case ((h, t, _)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }


  lazy val txTypeAccountTxIds: Map[(TransactionType.Value, Account), List[ByteStr]] = {
    val map: List[((TransactionType.Value, Account), Set[(Int, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => (tx.transaction.transactionType, acc) -> Set((h, tx.transaction.timestamp, id))) }
    val groupedByTuple = map.foldLeft(Map.empty[(TransactionType.Value, Account), Set[(Int, Long, ByteStr)]]) { case (m, (tuple, set)) =>
      m.combine(Map(tuple -> set))
    }
    groupedByTuple
      .mapValues(l => l.toList.sortBy { case ((h, t, _)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }

  lazy val accountContractIds: Map[Address, List[ByteStr]] = {
    val map: List[(Address, Set[(Int, ByteStr)])] = contracts.toList
      .flatMap { case (id, (h, _, _, accs)) => accs.map(acc => acc -> Set((h, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Address, Set[(Int, ByteStr)]]) { case (m, (acc, set)) =>
      m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case ((h, _)) => (-h) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._2))
  }
}

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio] = Map.empty,
            assetInfos: Map[ByteStr, AssetInfo] = Map.empty,
            slotids: Map[Int, Option[String]] = Map.empty,
            addToSlot: Map[String, Option[Int]] = Map.empty,
            slotNum: Int = 0,
            txStatus: TransactionStatus.Value = TransactionStatus.Success,
            chargedFee: Long = 0,
            contracts: Map[ByteStr, (Int, ByteStr, Contract, Set[Address])] = Map.empty,
            contractDB: Map[ByteStr, Array[Byte]] = Map.empty,
            contractTokens: Map[ByteStr, Int] = Map.empty,
            tokenDB: Map[ByteStr, Array[Byte]] = Map.empty,
            tokenAccountBalance: Map[ByteStr, Long] = Map.empty,
            relatedAddress: Map[Address, Boolean] = Map.empty,
            dbEntries: Map[ByteStr, Entry] = Map.empty,
            orderFills: Map[ByteStr, OrderFillInfo] = Map.empty,
            leaseState: Map[ByteStr, Boolean] = Map.empty): Diff = Diff(
    transactions = Map((tx.id, (height, ProcessedTransaction(txStatus, chargedFee, tx), (portfolios.keys ++ relatedAddress.keys).toSet))),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    slotids = slotids,
    addToSlot = addToSlot,
    slotNum = slotNum,
    txStatus = txStatus,
    chargedFee = chargedFee,
    contracts = contracts,
    contractDB = contractDB,
    contractTokens = contractTokens,
    tokenDB = tokenDB,
    tokenAccountBalance = tokenAccountBalance,
    dbEntries = dbEntries,
    orderFills = orderFills,
    leaseState = leaseState)

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, 0,
    TransactionStatus.Unprocessed, 0L, Map.empty, Map.empty,
    Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Map.empty)
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = older.transactions ++ newer.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
      slotids = older.slotids ++ newer.slotids,
      addToSlot = older.addToSlot ++ newer.addToSlot,
      slotNum = older.slotNum + newer.slotNum,
      txStatus = newer.txStatus,
      chargedFee = newer.chargedFee,
      contracts = older.contracts ++ newer.contracts,
      contractDB = older.contractDB ++ newer.contractDB,
      contractTokens = Monoid.combine(older.contractTokens, newer.contractTokens),
      tokenDB = older.tokenDB ++ newer.tokenDB,
      tokenAccountBalance = Monoid.combine(older.tokenAccountBalance, newer.tokenAccountBalance),
      dbEntries = older.dbEntries ++ newer.dbEntries,
      orderFills = older.orderFills.combine(newer.orderFills),
      leaseState = older.leaseState ++ newer.leaseState)
  }
}
