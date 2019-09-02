package vsys.blockchain.state.diffs

import vsys.blockchain.contract.ExecutionContext
import vsys.blockchain.state.opcdiffs.OpcFuncDiffer
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.{TransactionStatus, ValidationError}
import vsys.blockchain.transaction.ValidationError._

object ExecuteContractFunctionTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: ExecuteContractFunctionTransaction): Either[ValidationError, Diff] = {

    tx.proofs.proofs.headOption match {
      case Some(proofsHead) => {
        EllipticCurve25519Proof.fromBytes(proofsHead.bytes.arr).flatMap( proof => {
          val sender = proof.publicKey
          (for {
            exContext <- ExecutionContext.fromExeConTx(s, height, tx)
            diff <- OpcFuncDiffer(exContext)(tx.data)
          } yield (diff, sender)) match {
            case Right((df, sender)) => Right(Diff(
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
                case ce: ContractValidationError  => ce.transactionStatus
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
        })
      }
      case _ => Left(EmptyProofs)
    }
  }
}
