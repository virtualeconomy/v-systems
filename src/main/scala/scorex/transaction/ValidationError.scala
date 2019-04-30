package scorex.transaction

import scorex.account.{Address, Alias}
import scorex.transaction.assets.exchange.Order

trait ValidationError

object ValidationError {

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
  case object InvalidContract extends ValidationError
  case object InvalidContractAddress extends ValidationError
  case object InvalidDataEntry extends ValidationError
  case object InvalidFunctionIndex extends ValidationError
  case object ContractDataTypeMissMatch extends ValidationError
  case object ContractInvalidStateVariable extends ValidationError
  case object ContractStateVariableNotDefined extends ValidationError
  case object ContractInvalidOPCData extends ValidationError
  case object ContractUnsupportedOPC extends ValidationError
  case object ContractInvalidFunction extends ValidationError
  case object ContractInvalidTokenIndex extends ValidationError
  case object ContractInvalidAmount extends ValidationError
  case object ContractLocalVariableIndexOutOfRange extends ValidationError
  case object ContractTokenBalanceInsufficient extends ValidationError
  case object ContractTokenMaxExceeded extends ValidationError
  case object ContractInvalidTokenInfo extends ValidationError
  case object ContractUnsupportedWithdraw extends ValidationError
  case object ContractUnsupportedDeposit extends ValidationError
  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError
  case class AliasNotExists(a: Alias) extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError
  case class GenericError(err: String) extends ValidationError
  case class DbDataTypeError(err: String) extends ValidationError
  case class Mistiming(err: String) extends ValidationError
  case class WrongFeeScale(errFeeScale: Short) extends ValidationError
  case class WrongMintingReward(errReward: Long) extends ValidationError
  case class InvalidSlotId(errSlotId: Int) extends ValidationError
}
