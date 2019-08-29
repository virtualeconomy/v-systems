package vsys.blockchain.state.diffs

import vsys.blockchain.contract.ExecutionContext
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.state.opcdiffs.OpcFuncDiffer
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.blockchain.transaction.{TransactionStatus, ValidationError}
import vsys.blockchain.transaction.ValidationError._

import scala.util.Left

object RegisterContractTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: RegisterContractTransaction): Either[ValidationError, Diff] = {
    /**
      no need to validate the name duplication coz that will create a duplicate transacion and
      will fail with duplicated transaction id
    */
    if (tx.proofs.proofs.length > Proofs.MaxProofs) {
      return Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).flatMap( proof => {
      val sender = proof.publicKey
      val contractInfo = (height, tx.id, tx.contract, Set(sender.toAddress))
      ( for {
        exContext <- ExecutionContext.fromRegConTx(s, height, tx)
        diff <- OpcFuncDiffer(exContext)(tx.data)
      } yield diff) match {
        case Right(df) => Right(Diff(
          height = height,
          tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          contracts = Map(tx.contractId.bytes -> contractInfo),
          contractDB = df.contractDB,
          contractTokens = df.contractTokens,
          tokenDB = df.tokenDB,
          tokenAccountBalance = df.tokenAccountBalance,
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
}
