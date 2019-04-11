package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

import scala.language.higherKinds

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit

}

class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with ScorexLogging {

  import StateStorage._
  import StateWriterImpl._

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write { implicit l =>
    val txsDiff = blockDiff.txsDiff

    log.debug(s"Starting persist from ${sp().getHeight} to ${sp().getHeight + blockDiff.heightDiff}")

    measureSizeLog("transactions")(txsDiff.transactions) {
      _.par.foreach { case (id, (h, tx, _)) =>
        sp().transactions.put(id, (h, tx.bytes))
      }
    }

    measureSizeLog("orderFills")(blockDiff.txsDiff.orderFills) {
      _.par.foreach { case (oid, orderFillInfo) =>
        sp().orderFills.get(oid) match {
          case Some(ll) =>
            sp().orderFills.put(oid, (ll._1 + orderFillInfo.volume, ll._2 + orderFillInfo.fee))
          case None =>
            sp().orderFills.put(oid, (orderFillInfo.volume, orderFillInfo.fee))
        }
      }
    }

    measureSizeLog("portfolios")(txsDiff.portfolios) {
      _.foreach { case (account, portfolioDiff) =>
        val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
        sp().portfolios.put(account.bytes,
          (updatedPortfolio.balance,
            (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
            updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
      }
    }


    measureSizeLog("assets")(txsDiff.issuedAssets) {
      _.foreach { case (id, assetInfo) =>
        val updated = (sp().assets.get(id) match {
          case None => Monoid[AssetInfo].empty
          case Some(existing) => AssetInfo(existing._1, existing._2)
        }).combine(assetInfo)

        sp().assets.put(id, (updated.isReissuable, updated.volume))
      }
    }

    measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
      _.foreach { case (acc, txIds) =>
        val startIdxShift = sp().accountTransactionsLengths.get(acc.bytes).getOrElse(0)
        txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
          sp().accountTransactionIds.put(accountIndexKey(acc, shift), txId)
          shift + 1
        }
        sp().accountTransactionsLengths.put(acc.bytes, startIdxShift + txIds.length)
      }
    }

    measureSizeLog("txTypeAccountTxIds")(blockDiff.txsDiff.txTypeAccountTxIds) {
      _.foreach { case ((txType, acc), txIds) =>
        val startIdxShift = sp().txTypeAccTxLengths.get(txTypeAccKey(txType, acc)).getOrElse(0)
        txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
          sp().txTypeAccountTxIds.put(txTypeAccIndexKey(txType, acc, shift), txId)
          shift + 1
        }
        sp().txTypeAccTxLengths.put(txTypeAccKey(txType, acc), startIdxShift + txIds.length)
      }
    }

    measureSizeLog("effectiveBalanceSnapshots")(blockDiff.snapshots)(
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          sp().balanceSnapshots.put(accountIndexKey(acc, h), (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance, snapshot.weightedBalance))
        }
        sp().lastBalanceSnapshotHeight.put(acc.bytes, snapshotsByHeight.keys.max)
        sp().lastBalanceSnapshotWeightedBalance.put(acc.bytes, snapshotsByHeight(snapshotsByHeight.keys.max).weightedBalance)
      })

    measureSizeLog("aliases")(blockDiff.txsDiff.aliases) {
      _.foreach { case (alias, acc) =>
        sp().aliasToAddress.put(alias.name, acc.bytes)
      }
    }

    measureSizeLog("contracts")(blockDiff.txsDiff.contracts) {
      _.foreach { case (name, (status, account, content)) =>
        sp().contracts.put(name, (status, account.bytes, content))
      }
    }

    measureSizeLog("dbEntries")(blockDiff.txsDiff.dbEntries) {
      _.foreach { case (key, value) =>
        sp().dbEntries.put(key, value.bytes)
      }
    }

    measureSizeLog("lease info")(blockDiff.txsDiff.leaseState)(
      _.foreach { case (id, isActive) => sp().leaseState.put(id, isActive) })


    // if the blockDiff has contend/release transaction issued, change the slot address
    measureSizeLog("slotids_info")(blockDiff.txsDiff.slotids)(
      _.foreach {
        case (id, acc) => acc match {
          case None => sp().releaseSlotAddress(id)
          case _ => sp().setSlotAddress(id, acc.get)
        }
      })

    measureSizeLog("address to slot_info")(blockDiff.txsDiff.addToSlot)(
      _.foreach {
        case (acc, id) => id match {
          case None => sp().releaseAddressSlot(acc)
          case _ => sp().setAddressSlot(acc, id.get)
        }
      })

    sp().setHeight(sp().getHeight + blockDiff.heightDiff)

    log.debug("BlockDiff commit complete")
  }

  override def clear(): Unit = write { implicit l =>
    sp().removeEverything()
  }

}

object StateWriterImpl extends ScorexLogging {

  private def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }

  def measureSizeLog[F[_] <: TraversableOnce[_], A, R](s: String)(fa: => F[A])(f: F[A] => R): R = {
    val (r, time) = withTime(f(fa))
    log.debug(s"processing of ${fa.size} $s took ${time}ms")
    r
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (r, time) = withTime(f)
    log.trace(s"$s took ${time}ms")
    r
  }
}

