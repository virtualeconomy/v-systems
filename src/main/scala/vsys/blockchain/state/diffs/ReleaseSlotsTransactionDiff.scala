package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.spos.ReleaseSlotsTransaction
import vsys.settings.FunctionalitySettings

import scala.util.{Left, Right}
import vsys.blockchain.transaction.ValidationError.{GenericError, EmptyProofs}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.consensus.SPoSCalc._

object ReleaseSlotsTransactionDiff {
  def apply(s: StateReader, fs: FunctionalitySettings, height: Int)(tx: ReleaseSlotsTransaction): Either[ValidationError, Diff] = {
    /**
      check the slot list, make sure it is not the last miner (set a min num of miner)
      set the min num to half of total num of slots
    */
    tx.proofs.proofs.headOption match {
      case Some(proofsHead) => {
        EllipticCurve25519Proof.fromBytes(.bytes.arr).flatMap(proof => {
          val sender = proof.publicKey

          val MinimalSlotNumber = 10

          val hasEnoughMiner = s.effectiveSlotAddressSize > MinimalSlotNumber

          val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)

          val isValidAddress = s.slotAddress(tx.slotId) match {
            case Some(l) if l == sender.toAddress.address => true
            case _ => false
          }

          // add more ValidationError

          if (hasEnoughMiner && isValidAddress && isValidSlotID) {
            Right(Diff(
              height = height,
              tx = tx,
              portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
              slotids = Map(tx.slotId -> None),
              addToSlot = Map(sender.toAddress.address -> None),
              slotNum = -1,
              chargedFee = tx.fee
            ))
          }
          else if (!isValidSlotID) {
            Left(ValidationError.InvalidSlotId(tx.slotId))
          }
          else if (!hasEnoughMiner) {
            Left(GenericError(s"${s.effectiveSlotAddressSize} effective slot address(es) left, can not release the minting right"))
          }
          else {
            Left(GenericError(s"${sender.address} can not release the minting right of slot id: ${tx.slotId}"))
          }
        })
      }
      case _ => Left(EmptyProofs)
    }
  }
}
