package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.spos.ReleaseSlotsTransaction
import vsys.settings.FunctionalitySettings

import scala.util.{Left, Right}
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.consensus.SPoSCalc._

object ReleaseSlotsTransactionDiff {
  def apply(s: StateReader, fs: FunctionalitySettings, height: Int)(tx: ReleaseSlotsTransaction): Either[ValidationError, Diff] = {
    /**
      check the slot list, make sure it is not the last miner (set a min num of miner)
      set the min num to half of total num of slots
    */
    val MinimalSlotNumber = 10
    for {
      proof <- tx.proofs.firstCurveProof
      senderAddr = proof.publicKey.toAddress
      // check if enough minter
      _ <- if (s.effectiveSlotAddressSize <= MinimalSlotNumber) {
        Left(GenericError(s"${s.effectiveSlotAddressSize} effective slot address(es) left, can not release the minting right"))
      } else Right(())
      // check if slotId valid
      _ <- if (tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)) {
        Right(())
      } else Left(ValidationError.InvalidSlotId(tx.slotId))
      isValidAddress = s.slotAddress(tx.slotId) match {
        case Some(l) if l == senderAddr.address => true
        case _ => false
      }
      // check if address valid
      _ <- if (isValidAddress) Right(()) else {
        Left(GenericError(s"${senderAddr.address} can not release the minting right of slot id: ${tx.slotId}"))
      }
    } yield Diff(
      height = height,
      tx = tx,
      portfolios = Map(senderAddr -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      slotids = Map(tx.slotId -> None),
      addToSlot = Map(senderAddr.address -> None),
      slotNum = -1,
      chargedFee = tx.fee
    )
  }
}
