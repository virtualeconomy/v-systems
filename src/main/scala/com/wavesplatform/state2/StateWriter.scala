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
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with AutoCloseable with ScorexLogging {

  import StateStorage._
  import StateWriterImpl._

  override def close(): Unit = p.close()

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
        Option(sp().orderFills.get(oid)) match {
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
        val updated = (Option(sp().assets.get(id)) match {
          case None => Monoid[AssetInfo].empty
          case Some(existing) => AssetInfo(existing._1, existing._2)
        }).combine(assetInfo)

        sp().assets.put(id, (updated.isReissuable, updated.volume))
      }
    }

    measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
      _.foreach { case (acc, txIds) =>
        val startIdxShift = sp().accountTransactionsLengths.getOrDefault(acc.bytes, 0)
        txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
          sp().accountTransactionIds.put(accountIndexKey(acc, shift), txId)
          shift + 1
        }
        sp().accountTransactionsLengths.put(acc.bytes, startIdxShift + txIds.length)
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
      _.foreach { case (id, (h, txId, contract, _)) =>
        sp().contracts.put(id, (h, txId, contract.bytes.arr))
      }
    }

    measureSizeLog("accountContractIds")(blockDiff.txsDiff.accountContractIds) {
      _.foreach { case (acc, ctIds) =>
        val startIdxShift = sp().accountContractsLengths.getOrDefault(acc.bytes, 0)
        ctIds.reverse.foldLeft(startIdxShift) { case (shift, ctId) =>
          sp().accountContractIds.put(accountIndexKey(acc, shift), ctId)
          shift + 1
        }
        sp().accountContractsLengths.put(acc.bytes, startIdxShift + ctIds.length)
      }
    }

    measureSizeLog("contractDB")(blockDiff.txsDiff.contractDB) {
      _.foreach { case (id, contractData) =>
        sp().contractDB.put(id, contractData)
      }
    }

    measureSizeLog("contractTokens")(blockDiff.txsDiff.contractTokens) {
      _.foreach { case (id, tokenNum) =>
        val updatedNum = contractTokens(id) + tokenNum
        sp().contractTokens.put(id, updatedNum)
      }
    }

    measureSizeLog("tokenDB")(blockDiff.txsDiff.tokenDB) {
      _.foreach { case (id, tokeninfo) =>
        sp().tokenDB.put(id, tokeninfo)
      }
    }

    measureSizeLog("tokenAccountBalance")(blockDiff.txsDiff.tokenAccountBalance) {
      _.foreach { case (id, balance) =>
        val updatedBalance = safeSum(tokenAccountBalance(id), balance)
        sp().tokenAccountBalance.put(id, updatedBalance)
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
        case (id, acc) => acc.length match {
          case 0 => sp().releaseSlotAddress(id)
          case _ => sp().setSlotAddress(id, acc)
        }
      })

    sp().setHeight(sp().getHeight + blockDiff.heightDiff)
    sp().commit()

    log.debug("BlockDiff commit complete")
  }

  override def clear(): Unit = write { implicit l =>
    sp().transactions.clear()
    sp().portfolios.clear()
    sp().assets.clear()
    sp().accountTransactionIds.clear()
    sp().accountTransactionsLengths.clear()
    sp().balanceSnapshots.clear()
    sp().orderFills.clear()
    sp().aliasToAddress.clear()
    sp().leaseState.clear()
    sp().lastBalanceSnapshotHeight.clear()
    sp().lastBalanceSnapshotWeightedBalance.clear()
    sp().addressList.clear()
    sp().addressToID.clear()
    sp().dbEntries.clear()
    sp().contracts.clear()
    sp().accountContractIds.clear()
    sp().accountContractsLengths.clear()
    sp().contractDB.clear()
    sp().contractTokens.clear()
    sp().tokenDB.clear()
    sp().tokenAccountBalance.clear()
    sp().setHeight(0)
    sp().commit()
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
    log.trace(s"processing of ${fa.size} $s took ${time}ms")
    r
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (r, time) = withTime(f)
    log.trace(s"$s took ${time}ms")
    r
  }
}

