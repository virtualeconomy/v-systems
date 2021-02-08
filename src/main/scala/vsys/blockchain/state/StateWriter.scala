package vsys.blockchain.state

import java.util.concurrent.locks.ReentrantReadWriteLock
import cats.implicits._
import vsys.account.Address
import vsys.blockchain.state.reader.StateReaderImpl
import vsys.utils.ScorexLogging
import vsys.settings.StateSettings

import scala.language.higherKinds


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit

}

class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock, stateSettings: StateSettings)
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

    measureSizeLog("portfolios")(txsDiff.portfolios) {
      _.foreach { case (account, portfolioDiff) =>
        val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
        sp().portfolios.put(account.bytes,
          (updatedPortfolio.balance,
            (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
            updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
      }
    }

    measureSizeLog("accountTransactionIds")(blockDiff.txsDiff.accountTransactionIds) {
      _.foreach { case (acc, txIds) => if (stateSettings.txContractTxIds || acc.bytes.arr(0) == Address.AddressVersion) {
        val startIdxShift = sp().accountTransactionsLengths.get(acc.bytes).getOrElse(0)
        txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
          sp().accountTransactionIds.put(accountIndexKey(acc, shift), txId)
          shift + 1 
        }
        sp().accountTransactionsLengths.put(acc.bytes, startIdxShift + txIds.length)
      }
      }
    }

    if(stateSettings.txTypeAccountTxIds) {
      measureSizeLog("txTypeAccountTxIds")(blockDiff.txsDiff.txTypeAccountTxIds) {
        _.foreach { case ((txType, acc), txIds) =>
          if (stateSettings.txContractTxIds || acc.bytes.arr(0) == Address.AddressVersion) {
            val startIdxShift = sp().txTypeAccTxLengths.get(txTypeAccKey(txType, acc)).getOrElse(0)
            txIds.reverse.foldLeft(startIdxShift) { case (shift, txId) =>
              sp().txTypeAccountTxIds.put(txTypeAccIndexKey(txType, acc, shift), txId)
              shift + 1
            }
            sp().txTypeAccTxLengths.put(txTypeAccKey(txType, acc), startIdxShift + txIds.length)
          }
        }
      }
    }
    
    measureSizeLog("effectiveBalanceSnapshots")(blockDiff.snapshots) {
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          sp().balanceSnapshots.put(accountIndexKey(acc, h), (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance, snapshot.weightedBalance))
        }
        sp().lastBalanceSnapshotHeight.put(acc.bytes, snapshotsByHeight.keys.max)
        sp().lastBalanceSnapshotWeightedBalance.put(acc.bytes, snapshotsByHeight(snapshotsByHeight.keys.max).weightedBalance)
      }
    }

    measureSizeLog("contracts")(blockDiff.txsDiff.contracts) {
      _.foreach { case (id, (h, txId, contract, _)) =>
        sp().contracts.put(id, (h, txId, contract.bytes.arr))
      }
    }

    measureSizeLog("accountContractIds")(blockDiff.txsDiff.accountContractIds) {
      _.foreach { case (acc, ctIds) =>
        val startIdxShift = sp().accountContractsLengths.get(acc.bytes).getOrElse(0)
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

    measureSizeLog("contractNumDB")(blockDiff.txsDiff.contractNumDB) {
      _.foreach { case (id, dValue) =>
        val updatedNum = safeSum(contractNumInfo(id), dValue)
        sp().contractNumDB.put(id, updatedNum)
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

