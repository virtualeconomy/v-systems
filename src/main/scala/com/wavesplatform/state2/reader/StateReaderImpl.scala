package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.TransactionParser.TransactionType
import vsys.transaction.{ProcessedTransaction, ProcessedTransactionParser}

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends StateReader {

  val sp = Synchronized(p)

  override def transactionInfo(id: ByteStr): Option[(Int, ProcessedTransaction)] = read { implicit l =>
    sp().transactions.get(id).map {
      case (h, bytes) => (h, ProcessedTransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    sp().portfolios.get(a.bytes).map { case (b, (i, o), as) => Portfolio(b, LeaseInfo(i, o), as.map { case (k, v) => ByteStr(k) -> v }) }.orEmpty
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    Option(sp().assets.get(id).get).map {
      case (is, amt) => AssetInfo(is, amt)
    }
  }

  override def height: Int = read { implicit l => sp().getHeight }

  override def slotAddress(id: Int): Option[String] = read {implicit l => sp().getSlotAddress(id)}

  override def effectiveSlotAddressSize: Int = read {implicit l=> sp().getEffectiveSlotAddressSize}

  override def addressSlot(add: String): Option[Int] = read {implicit l=> sp().getAddressSlot(add)}

  override def accountTransactionIds(a: Address, limit: Int, offset: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().accountTransactionsLengths.get(a.bytes).getOrElse(0)
    Range(Math.max(0, totalRecords - limit - offset), Math.max(0, totalRecords - offset))
      .map(n => sp().accountTransactionIds.get(StateStorage.accountIndexKey(a, n)).get)
      .reverse
  }

  override def txTypeAccountTxIds(txType: TransactionType.Value, a: Address, limit: Int, offset: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().txTypeAccTxLengths.get(StateStorage.txTypeAccKey(txType, a)).getOrElse(0)
    Range(Math.max(0, totalRecords - limit - offset), Math.max(0, totalRecords - offset))
      .map(n => sp().txTypeAccountTxIds.get(StateStorage.txTypeAccIndexKey(txType, a, n)).get)
      .reverse
  }

  override def aliasesOfAddress(a: Address): Seq[Alias] = read { implicit l =>
    sp().aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Address] = read { implicit l =>
    sp().aliasToAddress.get(a.name)
      .map(b => Address.fromBytes(b.arr).explicitGet())
  }

  override def contractContent(name: String): Option[(Boolean, ByteStr, String)] = read { implicit l =>
    sp().contracts.get(name)
  }

  override def dbGet(key: ByteStr): Option[ByteStr] = read { implicit l =>
    sp().dbEntries.get(key)
  }

  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    sp().portfolios.asScala.map {
      case (acc, (b, (i, o), as)) => Address.fromBytes(acc.arr).explicitGet() -> Portfolio(b, LeaseInfo(i, o), as.map {
        case (k, v) => ByteStr(k) -> v
      })
    }.toMap
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().leaseState.get(leaseTx.id).getOrElse(false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    sp().lastBalanceSnapshotHeight.get(acc.bytes)
  }

  override def lastUpdateWeightedBalance(acc: Address): Option[Long] = read {implicit l =>
    sp().lastBalanceSnapshotWeightedBalance.get(acc.bytes)
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    sp().balanceSnapshots.get(StateStorage.accountIndexKey(acc, h))
      .map { case (ph, b, eb, wb) => Snapshot(ph, b, eb, wb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().transactions.containsKey(id)
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { _ =>
    p.orderFills.get(orderId).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }
}
