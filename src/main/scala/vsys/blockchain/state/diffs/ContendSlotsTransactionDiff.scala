package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.TransactionStatus
import vsys.blockchain.transaction.spos.ContendSlotsTransaction
import vsys.account.Address
import vsys.settings.FunctionalitySettings
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.blockchain.consensus.SPoSCalc._

import scala.util.Right
import scala.util.Left

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {

    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val proofLength = tx.proofs.proofs.length

    val multiSlotsCheck = s.addressSlot(sender.address) match {
      case None => false
      case _ => true
    }
    val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)

    if (proofLength > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else if (!multiSlotsCheck && isValidSlotID){
      val contendEffectiveBalance = s.accountPortfolio(sender.toAddress).effectiveBalance
      // check effective balance after contend
      if (contendEffectiveBalance - tx.fee < MinimalEffectiveBalanceForContender) {
        // charge the transaction fee without any modification
        Right(Diff(height = height, tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          txStatus = TransactionStatus.ContendFailed,
          chargedFee = tx.fee))
      }
      else {
        val contendGen = mintingBalance(s, fs, sender.toAddress, height)
        val (slotGen, slotIncrease) = s.slotAddress(tx.slotId) match {
          //if the slot is occupied, return the generating balance, else return 0
          case Some(l) => (Address.fromString(l).right.map(arr => mintingBalance(s, fs, arr, height)).getOrElse(0L), 0)
          case None => (0L, 1) //here 0 can be changed to min contend MAB
        }
        if (contendGen > slotGen) {
          // charge transaction fee and contend the slot
          Right(Diff(height = height, tx = tx,
            portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
            slotids = Map(tx.slotId -> Option(sender.toAddress.address)),
            addToSlot = Map(sender.toAddress.address -> Option(tx.slotId)),
            slotNum = slotIncrease,
            chargedFee = tx.fee))
        }
        else {
          // charge the transaction fee without any modification
          Right(Diff(height = height, tx = tx,
            portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
            txStatus = TransactionStatus.ContendFailed,
            chargedFee = tx.fee
          ))
        }
      }
    }
    else if (multiSlotsCheck){
      Left(GenericError(s"${sender.address} already owned one slot or contended by other node"))
    }
    else{
      Left(ValidationError.InvalidSlotId(tx.slotId))
    }
  }
}
