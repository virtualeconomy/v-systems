package vsys.utils

import vsys.blockchain.transaction.assets._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.database._
import vsys.blockchain.transaction.spos._
import vsys.blockchain.transaction.lease._
import vsys.blockchain.transaction._
import vsys.blockchain.transaction.proof._

object TransactionHelper {

  def extractAddresses(tx: ProcessedTransaction): Seq[String] = {
    tx.transaction match {
      case p: PaymentTransaction => Seq(p.recipient.toString, getProofs(p.proofs))
      case b: BurnTransaction => Seq(b.sender.toString)
      case i: IssueTransaction => Seq(i.sender.toString)
      case r: ReissueTransaction => Seq(r.sender.toString)
      case t: TransferTransaction => Seq(t.sender.toString)
      case dbTx: DbPutTransaction => Seq(getProofs(dbTx.proofs))
      case lct: LeaseCancelTransaction => Seq(getProofs(lct.proofs))
      case cst: ContendSlotsTransaction => Seq(getProofs(cst.proofs))
      case rst: ReleaseSlotsTransaction => Seq(getProofs(rst.proofs))
      case lt: LeaseTransaction => Seq(lt.recipient.toString, getProofs(lt.proofs))
      case gt: GenesisTransaction => Seq(gt.recipient.toString)
      case mt: MintingTransaction => Seq(mt.recipient.toString)
      case ecft: ExecuteContractFunctionTransaction => Seq(ecft.contractId.toString, getProofs(ecft.proofs))
      case rct: RegisterContractTransaction => Seq(rct.contract.stringRepr, getProofs(rct.proofs))
      case _ => Seq.empty
    }
  }

  def extractAmtFee(tx: ProcessedTransaction): (Long, Long) = {
    tx.transaction match {
      case p: PaymentTransaction => (p.amount, p.fee)
      case b: BurnTransaction => (b.amount, b.fee)
      case i: IssueTransaction => (0, i.fee)
      case r: ReissueTransaction => (0, r.fee)
      case t: TransferTransaction => (t.amount, t.fee)
      case dbTx: DbPutTransaction => (0, dbTx.fee)
      case lct: LeaseCancelTransaction => (0, lct.fee)
      case cst: ContendSlotsTransaction => (0, cst.fee)
      case rst: ReleaseSlotsTransaction => (0, rst.fee)
      case lt: LeaseTransaction => (lt.amount, lt.fee)
      case gt: GenesisTransaction => (gt.amount, 0)
      case mt: MintingTransaction => (mt.amount, 0)
      case ecft: ExecuteContractFunctionTransaction => (0, ecft.fee)
      case rct: RegisterContractTransaction => (0, rct.fee)
      case _ => (0, 0)
    }
  }

  private def getProofs(proofs: Proofs): String = {
    proofs.firstCurveProof.map(_.publicKey.toString) match {
      case Right(s) => s
      case Left(e) => ""
    }
  }
}