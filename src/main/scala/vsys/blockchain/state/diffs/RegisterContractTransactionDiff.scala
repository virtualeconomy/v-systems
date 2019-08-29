package vsys.blockchain.state.diffs

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.account.PublicKeyAccount
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.ExecutionContext
import vsys.blockchain.state.opcdiffs.OpcFuncDiffer
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.TransactionStatus
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.Left

object RegisterContractTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: RegisterContractTransaction): Either[ValidationError, Diff] = {
    /**
      no need to validate the name duplication coz that will create a duplicate transacion and
      will fail with duplicated transaction id
    */
    if (tx.proofs.proofs.length > Proofs.MaxProofs) {
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else {
      EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).flatMap( proof =>
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
          case Left(e) => Right(toDiff(height, tx, sender)(fromValidationError(e)))
        }
      )
    }
  }

  private def fromValidationError(e: ValidationError): TransactionStatus.Value = e match {
    case InvalidContract => TransactionStatus.InvalidContract
    case InvalidContractAddress => TransactionStatus.InvalidContractAddress
    case InvalidDataEntry => TransactionStatus.InvalidDataEntry
    case ContractDataTypeMismatch => TransactionStatus.ContractDataTypeMismatch
    case ContractInvalidStateVariable => TransactionStatus.ContractInvalidStateVariable
    case ContractStateVariableNotDefined => TransactionStatus.ContractStateVariableNotDefined
    case ContractInvalidOPCData => TransactionStatus.ContractInvalidOPCData
    case ContractUnsupportedOPC => TransactionStatus.ContractUnsupportedOPC
    case ContractInvalidSigner => TransactionStatus.ContractInvalidSigner
    case ContractInvalidCaller => TransactionStatus.ContractInvalidCaller
    case ContractInvalidFunction => TransactionStatus.ContractInvalidFunction
    case ContractInvalidTokenIndex => TransactionStatus.ContractInvalidTokenIndex
    case ContractInvalidAmount => TransactionStatus.ContractInvalidAmount
    case ContractLocalVariableIndexOutOfRange => TransactionStatus.ContractLocalVariableIndexOutOfRange
    case ContractTokenBalanceInsufficient => TransactionStatus.ContractTokenBalanceInsufficient
    case ContractTokenMaxExceeded => TransactionStatus.ContractTokenMaxExceeded
    case ContractInvalidTokenInfo => TransactionStatus.ContractInvalidTokenInfo
    case ContractUnsupportedWithdraw => TransactionStatus.ContractUnsupportedWithdraw
    case ContractUnsupportedDeposit => TransactionStatus.ContractUnsupportedDeposit
    case _ => TransactionStatus.Failed
  }

  private def toDiff(height: Int, tx: RegisterContractTransaction, sender: PublicKeyAccount)(a: TransactionStatus.Value): Diff = {
    Diff(
      height = height,
      tx = tx,
      portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      chargedFee = tx.fee,
      txStatus = a
    )
  }

}
