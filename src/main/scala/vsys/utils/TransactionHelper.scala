package vsys.utils

import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.database._
import vsys.blockchain.transaction.spos._
import vsys.blockchain.transaction.lease._
import vsys.blockchain.transaction._

object TransactionHelper {

  def extractAmtFee(tx: ProcessedTransaction): (Long, Long) = {
    tx.transaction match {
      case p: PaymentTransaction => (p.amount, p.transactionFee)
      case dbTx: DbPutTransaction => (0, dbTx.transactionFee)
      case lct: LeaseCancelTransaction => (0, lct.transactionFee)
      case cst: ContendSlotsTransaction => (0, cst.transactionFee)
      case rst: ReleaseSlotsTransaction => (0, rst.transactionFee)
      case lt: LeaseTransaction => (lt.amount, lt.transactionFee)
      case gt: GenesisTransaction => (gt.amount, 0)
      case mt: MintingTransaction => (mt.amount, 0)
      case ecft: ExecuteContractFunctionTransaction => (0, ecft.transactionFee)
      case rct: RegisterContractTransaction => (0, rct.transactionFee)
      case _ => (0, 0)
    }
  }
}