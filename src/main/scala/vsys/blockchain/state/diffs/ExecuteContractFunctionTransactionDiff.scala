package vsys.blockchain.state.diffs

import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.state.reader.StateReader
import vsys.account.PublicKeyAccount
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.ExecutionContext
import vsys.blockchain.state.opcdiffs.OpcFuncDiffer
import vsys.blockchain.transaction.TransactionStatus
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}


object ExecuteContractFunctionTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: ExecuteContractFunctionTransaction): Either[ValidationError, Diff] = {
    (for {
      proof <-  EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr)
      sender = proof.publicKey
      exContext <- ExecutionContext.fromExeConTx(s, height, tx)
      diff <- OpcFuncDiffer(exContext)(tx.data)
    } yield diff) match {
      case Right(df) => Right(Diff(
        height = height,
        tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        tokenDB = df.tokenDB,
        tokenAccountBalance = df.tokenAccountBalance,
        contractDB = df.contractDB,
        contractTokens = df.contractTokens,
        relatedAddress = df.relatedAddress,
        chargedFee = tx.fee
      ))
      case Left(e) => {
        val status = e match {
          case ContractValidationError  => e.transactionStatus
          case _ => TransactionStatus.Failed
        }
        Right(Diff(
          height = height,
          tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          chargedFee = tx.fee,
          txStatus = status
        ))
      }
    }
  }
}
