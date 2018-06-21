package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.{ReleaseSlotsTransaction, ValidationError}
import com.wavesplatform.settings.FunctionalitySettings

import scala.util.{Left, Right}
import scorex.transaction.ValidationError.GenericError

object ReleaseSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ReleaseSlotsTransaction): Either[ValidationError, Diff] = {
    // TODO
    // check the slot list, make sure it is not the last miner (set a min num of miner)
    val isLastMiner = false

    val isValidSlotID = tx.slotid < fs.numOfSlots && tx.slotid >=0

    val isValidAddress = s.slotAddress(tx.slotid) match {
      case Some(l) if l == tx.sender.toAddress.address => true
      case _ => false
    }

    // add more ValidationError

    val emptyAddress = ""
    if (!isLastMiner && isValidAddress && isValidSlotID) {
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        slotids = Map(tx.slotid -> emptyAddress),slotNum = -1))
    }
    else if (!isValidSlotID){
      Left(GenericError(s"${tx.slotid} invalid."))
    }
    else{
      Left(GenericError(s"${tx.sender.address} can not release the mint right of slot id: ${tx.slotid}"))
    }
  }
}
