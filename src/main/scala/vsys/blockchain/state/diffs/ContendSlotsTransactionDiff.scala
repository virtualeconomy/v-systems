package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.TransactionStatus
import vsys.blockchain.transaction.spos.ContendSlotsTransaction
import vsys.account.Address
import vsys.settings.FunctionalitySettings
import vsys.blockchain.transaction.ValidationError.{EmptyProofs, GenericError}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.consensus.SPoSCalc._

import scala.util.Right
import scala.util.Left

object ContendSlotsTransactionDiff {
  def apply(s: StateReader,fs: FunctionalitySettings,height: Int)(tx: ContendSlotsTransaction): Either[ValidationError, Diff] = {
    tx.proofs.proofs.headOption match {
      case Some(proofsHead) => {
        EllipticCurve25519Proof.fromBytes(proofsHead.bytes.arr).flatMap( proof => {
          val sender = proof.publicKey
          val multiSlotsCheck = s.addressSlot(sender.address) match {
            case None => false
            case _ => true
          }
          val isValidSlotID = tx.slotId < fs.numOfSlots && tx.slotId >=0 && (tx.slotId % SlotGap == 0)

          if (multiSlotsCheck) {
            Left(GenericError(s"${sender.address} already owned one slot or contended by other node"))
          }
          else if (!isValidSlotID) {
            Left(ValidationError.InvalidSlotId(tx.slotId))
          }
          else {
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
        })
      }
      case _ => Left(EmptyProofs)
    }
  }
}
