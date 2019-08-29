package vsys.blockchain.transaction

import vsys.account.Address
import vsys.blockchain.transaction.assets.exchange.Order

trait ValidationError

object ValidationError {

  import vsys.blockchain.transaction.TransactionStatus

  trait ContractValidationError extends ValidationError {
    val transactionStatus: TransactionStatus.Value
  }

  case object InvalidAddress extends ValidationError
  case object InvalidDataType extends ValidationError
  case object InvalidDataLength extends ValidationError
  case class TooLongDbEntry(actualLength: Int, maxLength: Int) extends ValidationError
  case class InvalidUTF8String(field: String) extends ValidationError
  case object InvalidProofType extends ValidationError
  case object InvalidProofLength extends ValidationError
  case object InvalidProofBytes extends ValidationError
  case object NegativeAmount extends ValidationError
  case object InsufficientFee extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidName extends ValidationError
  case object InvalidDbKey extends ValidationError
  case object OverflowError extends ValidationError
  case object ToSelf extends ValidationError
  case object InvalidProcessedTransaction extends ValidationError
  case object MissingSenderPrivateKey extends ValidationError
  case object UnsupportedTransactionType extends ValidationError
  case object InvalidRequestSignature extends ValidationError
  case object InvalidContract extends ContractValidationError {
    override val transactionStatus = TransactionStatus.InvalidContract
  }
  case object InvalidContractAddress extends ContractValidationError {
    override val transactionStatus = TransactionStatus.InvalidContractAddress
  }
  case object InvalidDataEntry extends ValidationError {
    override val transactionStatus = TransactionStatus.InvalidDataEntry
  }
  case object InvalidFunctionIndex extends ValidationError {
    override val transactionStatus = TransactionStatus.InvalidFunctionIndex
  }
  case object ContractDataTypeMismatch extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractDataTypeMismatch
  }
  case object ContractInvalidStateVariable extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidStateVariable
  }
  case object ContractStateVariableNotDefined extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractStateVariableNotDefined
  }
  case object ContractInvalidOPCData extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidOPCData
  }
  case object ContractUnsupportedOPC extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractUnsupportedOPC
  }
  case object ContractInvalidSigner extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidSigner
  }
  case object ContractInvalidCaller extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidCaller
  }
  case object ContractInvalidFunction extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidFunction
  }
  case object ContractInvalidTokenIndex extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidTokenIndex
  }
  case object ContractInvalidAmount extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidAmount
  }
  case object ContractLocalVariableIndexOutOfRange extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractLocalVariableIndexOutOfRange
  }
  case object ContractTokenBalanceInsufficient extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractTokenBalanceInsufficient
  }
  case object ContractTokenMaxExceeded extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractTokenMaxExceeded
  }
  case object ContractInvalidTokenInfo extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractInvalidTokenInfo
  }
  case object ContractUnsupportedWithdraw extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractUnsupportedWithdraw
  }
  case object ContractUnsupportedDeposit extends ValidationError {
    override val transactionStatus = TransactionStatus.ContractUnsupportedDeposit
  }
  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError
  case class GenericError(err: String) extends ValidationError
  case class DbDataTypeError(err: String) extends ValidationError
  case class Mistiming(err: String) extends ValidationError
  case class WrongFeeScale(errFeeScale: Short) extends ValidationError
  case class WrongMintingReward(errReward: Long) extends ValidationError
  case class InvalidSlotId(errSlotId: Int) extends ValidationError
}
