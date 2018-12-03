package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import vsys.transaction.{ProcessedTransaction, ProcessedTransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends StateReader {

  val sp = Synchronized(p)

  override def transactionInfo(id: ByteStr): Option[(Int, ProcessedTransaction)] = read { implicit l =>
    Option(sp().transactions.get(id)).map {
      case (h, bytes) => (h, ProcessedTransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    Option(sp().portfolios.get(a.bytes)).map { case (b, (i, o), as) => Portfolio(b, LeaseInfo(i, o), as.map { case (k, v) => ByteStr(k) -> v }) }.orEmpty
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    Option(sp().assets.get(id)).map {
      case (is, amt) => AssetInfo(is, amt)
    }
  }

  override def height: Int = read { implicit l => sp().getHeight }

  override def slotAddress(id: Int): Option[String] = read {implicit l => sp().getSlotAddress(id)}

  override def effectiveSlotAddressSize: Int = read {implicit l=> sp().getEffectiveSlotAddressSize}

  override def addressToSlotID(add: String): Option[Int] = read {implicit l=> sp().addressToSlotID(add)}

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().accountTransactionsLengths.getOrDefault(a.bytes, 0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => sp().accountTransactionIds.get(StateStorage.accountIndexKey(a, n)))
      .reverse
  }

  override def aliasesOfAddress(a: Address): Seq[Alias] = read { implicit l =>
    sp().aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Address] = read { implicit l =>
    Option(sp().aliasToAddress.get(a.name))
      .map(b => Address.fromBytes(b.arr).explicitGet())
  }

  override def contractContent(name: String): Option[(Boolean, ByteStr, String)] = read { implicit l =>
    Option(sp().contracts.get(name))
  }

  override def dbGet(key: ByteStr): Option[ByteStr] = read { implicit l =>
    Option(sp().dbEntries.get(key))
  }

  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    sp().portfolios.asScala.map {
      case (acc, (b, (i, o), as)) => Address.fromBytes(acc.arr).explicitGet() -> Portfolio(b, LeaseInfo(i, o), as.map {
        case (k, v) => ByteStr(k) -> v
      })
    }.toMap
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().leaseState.getOrDefault(leaseTx.id, false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    Option(sp().lastBalanceSnapshotHeight.get(acc.bytes))
  }

  override def lastUpdateWeightedBalance(acc: Address): Option[Long] = read {implicit l =>
    Option(sp().lastBalanceSnapshotWeightedBalance.get(acc.bytes))
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    Option(sp().balanceSnapshots.get(StateStorage.accountIndexKey(acc, h)))
      .map { case (ph, b, eb, wb) => Snapshot(ph, b, eb, wb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().transactions.containsKey(id)
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { _ =>
    Option(p.orderFills.get(orderId)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }
}
