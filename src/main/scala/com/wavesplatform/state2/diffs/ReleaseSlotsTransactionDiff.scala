package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError
import vee.transaction.spos.ReleaseSlotsTransaction
import com.wavesplatform.settings.FunctionalitySettings

import scala.util.{Left, Right}
import scorex.transaction.ValidationError.GenericError

object ReleaseSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ReleaseSlotsTransaction): Either[ValidationError, Diff] = {
    // check the slot list, make sure it is not the last miner (set a min num of miner)
    // set the min num to half of total num of slots
    val hasEnoughMiner = s.effectiveSlotAddressSize >= (fs.numOfSlots + 1) / 2 && s.effectiveSlotAddressSize > 1

    val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0

    val isValidAddress = s.slotAddress(tx.slotId) match {
      case Some(l) if l == tx.sender.toAddress.address => true
      case _ => false
    }

    // add more ValidationError

    val emptyAddress = ""
    if (hasEnoughMiner && isValidAddress && isValidSlotID) {
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        slotids = Map(tx.slotId -> emptyAddress),slotNum = -1))
    }
    else if (!isValidSlotID){
      Left(GenericError(s"slot id: ${tx.slotId} invalid."))
    }
    else if (!hasEnoughMiner){
      Left(GenericError(s"${s.effectiveSlotAddressSize} effective slot address(es) left, can not release the minting right"))
    }
    else{
      Left(GenericError(s"${tx.sender.address} can not release the minting right of slot id: ${tx.slotId}"))
    }
  }
}
