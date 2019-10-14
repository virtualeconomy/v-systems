package vsys.utils

import vsys.blockchain.transaction._

object TransactionHelper {

  def extractAmtFee(tx: ProcessedTransaction): (Long, Long) = {
    tx.transaction match {
      case tx: NonFeeTransaction with AmountInvolved => (tx.amount, 0)
      case txWithFee: AmountInvolved => (txWithFee.amount, txWithFee.transactionFee)
      case pTx: ProvenTransaction => (0, pTx.transactionFee)
      case _ => (0, 0)
    }
  }
}