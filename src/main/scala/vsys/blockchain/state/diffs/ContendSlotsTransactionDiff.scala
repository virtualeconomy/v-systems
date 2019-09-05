package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.{TransactionStatus, ValidationError}
import vsys.blockchain.transaction.spos.ContendSlotsTransaction
import vsys.account.Address
import vsys.settings.FunctionalitySettings
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.consensus.SPoSCalc._

import scala.util.{Left, Right}

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {
    for {
      proof <- tx.proofs.firstCurveProof
      senderAddress: Address = proof.publicKey.toAddress
      // multi Slots Check
      _ <- if (s.addressSlot(senderAddress.address).isDefined) {
        Left(GenericError(s"${senderAddress.address} already owned one slot or contended by other node"))
      } else Right(())
      // valid slotId check
      _ <- if (tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)) {
        Right(())
      } else Left(ValidationError.InvalidSlotId(tx.slotId))
    } yield {
      val contendEffectiveBalance = s.accountPortfolio(senderAddress).effectiveBalance
      // charge the transaction fee without any modification
      val txFailDiff = Diff(
        height = height,
        tx = tx,
        portfolios = Map(senderAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        txStatus = TransactionStatus.ContendFailed,
        chargedFee = tx.fee
      )
      // check effective balance after contend
      if (contendEffectiveBalance - tx.fee < MinimalEffectiveBalanceForContender) txFailDiff
      else {
        val contendGen = mintingBalance(s, fs, senderAddress, height)
        val (slotGen, slotIncrease) = s.slotAddress(tx.slotId) match {
          //if the slot is occupied, return the generating balance, else return 0
          case Some(l) => (Address.fromString(l).right.map(arr => mintingBalance(s, fs, arr, height)).getOrElse(0L), 0)
          case None => (0L, 1) //here 0 can be changed to min contend MAB
        }
        if (contendGen > slotGen) {
          // charge transaction fee and contend the slot
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(senderAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
            slotids = Map(tx.slotId -> Option(senderAddress.address)),
            addToSlot = Map(senderAddress.address -> Option(tx.slotId)),
            slotNum = slotIncrease,
            chargedFee = tx.fee
          )
        }
        else txFailDiff
      }
    }
  }
}
