package vsys.blockchain.state.diffs

import cats.implicits._
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
    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    if (tx.proofs.proofs.length > Proofs.MaxProofs) {
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    } else {
      (for {
        exContext <- ExecutionContext.fromExeConTx(s, height, tx)
        diff <- OpcFuncDiffer(exContext)(tx.data)
      } yield diff)
    } match {
      case Right(df) => Right(Diff(height = height, tx = tx,
        portfolios = df.portfolios.combine(Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))), tokenDB = df.tokenDB,
        tokenAccountBalance = df.tokenAccountBalance, contractDB = df.contractDB, contractTokens = df.contractTokens,
        relatedAddress = df.relatedAddress, chargedFee = tx.fee))
      case Left(InvalidContract) => Right(toDiff(height, tx, sender)(fromValidationError(InvalidContract)))
      case Left(InvalidContractAddress) => Right(toDiff(height, tx, sender)(fromValidationError(InvalidContractAddress)))
      case Left(InvalidDataEntry) => Right(toDiff(height, tx, sender)(fromValidationError(InvalidDataEntry)))
      case Left(InvalidFunctionIndex) => Right(toDiff(height, tx, sender)(fromValidationError(InvalidFunctionIndex)))
      case Left(ContractDataTypeMismatch) => Right(toDiff(height, tx, sender)(fromValidationError(ContractDataTypeMismatch)))
      case Left(ContractInvalidStateVariable) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidStateVariable)))
      case Left(ContractStateVariableNotDefined) => Right(toDiff(height, tx, sender)(fromValidationError(ContractStateVariableNotDefined)))
      case Left(ContractInvalidOPCData) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidOPCData)))
      case Left(ContractUnsupportedOPC) => Right(toDiff(height, tx, sender)(fromValidationError(ContractUnsupportedOPC)))
      case Left(ContractInvalidSigner) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidSigner)))
      case Left(ContractInvalidCaller) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidCaller)))
      case Left(ContractInvalidFunction) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidFunction)))
      case Left(ContractInvalidTokenIndex) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidTokenIndex)))
      case Left(ContractInvalidAmount) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidAmount)))
      case Left(ContractLocalVariableIndexOutOfRange) => Right(toDiff(height, tx, sender)(fromValidationError(ContractLocalVariableIndexOutOfRange)))
      case Left(ContractTokenBalanceInsufficient) => Right(toDiff(height, tx, sender)(fromValidationError(ContractTokenBalanceInsufficient)))
      case Left(ContractTokenMaxExceeded) => Right(toDiff(height, tx, sender)(fromValidationError(ContractTokenMaxExceeded)))
      case Left(ContractInvalidTokenInfo) => Right(toDiff(height, tx, sender)(fromValidationError(ContractInvalidTokenInfo)))
      case Left(ContractUnsupportedWithdraw) => Right(toDiff(height, tx, sender)(fromValidationError(ContractUnsupportedWithdraw)))
      case Left(ContractUnsupportedDeposit) => Right(toDiff(height, tx, sender)(fromValidationError(ContractUnsupportedDeposit)))
      case Left(e) => Right(toDiff(height, tx, sender)(fromValidationError(e)))
    }
  }

  private def fromValidationError(e: ValidationError): TransactionStatus.Value = e match {
    case InvalidContract => TransactionStatus.InvalidContract
    case InvalidContractAddress => TransactionStatus.InvalidContractAddress
    case InvalidDataEntry => TransactionStatus.InvalidDataEntry
    case InvalidFunctionIndex => TransactionStatus.InvalidFunctionIndex
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

  private def toDiff(height: Int, tx: ExecuteContractFunctionTransaction, sender: PublicKeyAccount)(a: TransactionStatus.Value): Diff = {
    Diff(height = height, tx = tx,
      portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      chargedFee = tx.fee, txStatus = a)
  }

}
