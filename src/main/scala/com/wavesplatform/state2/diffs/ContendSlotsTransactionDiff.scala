package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.{ContendSlotsTransaction, ValidationError}
import scorex.transaction.PoSCalc._
import scorex.account.Address
import com.wavesplatform.settings.FunctionalitySettings
import scorex.transaction.ValidationError.GenericError

import scala.util.Right
import scala.util.Left

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {

    val MultiSlotsCheck = s.addressToSlotID(tx.sender.address).isDefined
    val isValidSlotID = tx.slotid < 5 && tx.slotid >=0

    if (!MultiSlotsCheck && isValidSlotID){
      val ContendGen = generatingBalance(s,fs,tx.sender,height)
      val SlotGen = s.slotAddress(tx.slotid) match {
        //if the slot is occupied, return the generating balance, else return 0
        case Some(l) => Address.fromString(l).right.map( arr => generatingBalance(s,fs,arr,height)).getOrElse(0L)
        case None => 0L //here 0 can be changed to min contend cost
      }
      if (ContendGen > SlotGen){
        // charge transaction fee and contend the slot
        Right(Diff(height = height, tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          slotids = Map(tx.slotid -> tx.sender.toAddress.address),
          slotNum = 1))
      }
      else {
        // charge the transaction fee without any modification
        // may return some warning
        Right(Diff(height = height, tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))))
      }
    }
    else if (MultiSlotsCheck){
      Left(GenericError(s"${tx.sender.address} already own one slot."))
    }
    else{
      Left(GenericError(s"${tx.slotid} invalid."))
    }
  }
}
