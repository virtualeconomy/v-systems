package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError
import vsys.transaction.spos.ReleaseSlotsTransaction
import com.wavesplatform.settings.FunctionalitySettings

import scala.util.{Left, Right}
import scorex.transaction.ValidationError.GenericError
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.spos.SPoSCalc._

object ReleaseSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ReleaseSlotsTransaction): Either[ValidationError, Diff] = {
    // check the slot list, make sure it is not the last miner (set a min num of miner)
    // set the min num to half of total num of slots
    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val proofLength = tx.proofs.proofs.length

    val MinimalSlotNumber = 10

    val hasEnoughMiner = s.effectiveSlotAddressSize > MinimalSlotNumber

    val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)

    val isValidAddress = s.slotAddress(tx.slotId) match {
      case Some(l) if l == sender.toAddress.address => true
      case _ => false
    }

    // add more ValidationError

    val emptyAddress = ""
    if (proofLength > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else if (hasEnoughMiner && isValidAddress && isValidSlotID) {
      Right(Diff(height = height, tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        slotids = Map(tx.slotId -> emptyAddress),
        addToSlot = Map(sender.toAddress.address -> -1),
        slotNum = -1, chargedFee = tx.fee))
    }
    else if (!isValidSlotID){
      Left(ValidationError.InvalidSlotId(tx.slotId))
    }
    else if (!hasEnoughMiner){
      Left(GenericError(s"${s.effectiveSlotAddressSize} effective slot address(es) left, can not release the minting right"))
    }
    else{
      Left(GenericError(s"${sender.address} can not release the minting right of slot id: ${tx.slotId}"))
    }
  }
}
