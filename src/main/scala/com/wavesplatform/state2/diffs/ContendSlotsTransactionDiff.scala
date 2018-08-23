package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError
import vee.transaction.spos.ContendSlotsTransaction
import vee.spos.SPoSCalc._
import scorex.account.Address
import com.wavesplatform.settings.FunctionalitySettings
import scorex.transaction.ValidationError.GenericError
import vee.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.Right
import scala.util.Left

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {

    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val proofLength = tx.proofs.proofs.length

    val MultiSlotsCheck = s.addressToSlotID(sender.address) match {
      case None => false
      case _ => true
    }
    val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0

    if (proofLength > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else if (!MultiSlotsCheck && isValidSlotID){
      val ContendGen = mintingBalance(s,fs,sender,height)
      val SlotGen = s.slotAddress(tx.slotId) match {
        //if the slot is occupied, return the generating balance, else return 0
        case Some(l) => Address.fromString(l).right.map( arr => mintingBalance(s,fs,arr,height)).getOrElse(0L)
        case None => 0L //here 0 can be changed to min contend cost
      }
      if (ContendGen > SlotGen){
        // charge transaction fee and contend the slot
        Right(Diff(height = height, tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          slotids = Map(tx.slotId -> sender.toAddress.address),
          slotNum = 1))
      }
      else {
        // charge the transaction fee without any modification
        // may return some warning
        Right(Diff(height = height, tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))))
      }
    }
    else if (MultiSlotsCheck){
      Left(GenericError(s"${sender.address} already own one slot."))
    }
    else{
      Left(GenericError(s"slot id: ${tx.slotId} invalid."))
    }
  }
}
