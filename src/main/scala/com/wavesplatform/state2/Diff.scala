package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Address, Alias}
import vsys.database.Entry
import scorex.transaction.Transaction
import vsys.contract.Contract
import vsys.transaction.{ProcessedTransaction, TransactionStatus}

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

case class Diff(transactions: Map[ByteStr, (Int, ProcessedTransaction, Set[Address])],
                portfolios: Map[Address, Portfolio],
                issuedAssets: Map[ByteStr, AssetInfo],
                aliases: Map[Alias, Address],
                slotids: Map[Int,String],
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

  lazy val accountTransactionIds: Map[Address, List[ByteStr]] = {
    val map: List[(Address, Set[(Int, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.transaction.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Address, Set[(Int, Long, ByteStr)]]) { case (m, (acc, set)) =>
      m.combine(Map(acc -> set))
    }
    groupedByAcc
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
            portfolios: Map[Address, Portfolio] = Map.empty,
            assetInfos: Map[ByteStr, AssetInfo] = Map.empty,
            aliases: Map[Alias, Address] = Map.empty,
            slotids: Map[Int,String] = Map.empty,
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
    aliases = aliases,
    slotids = slotids,
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
      aliases = older.aliases ++ newer.aliases,
      slotids = older.slotids ++ newer.slotids,
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
