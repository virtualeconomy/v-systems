package vsys.blockchain.contract

trait FuncDataStruct {
  val functionIndex: Int
  val signer: String
  val contractId: String
  val tokenId: String
}

trait FuncAmtInvolved {
  val amount: Long
}

object FuncDataStruct {}
object FuncAmtInvolved {}

case class SupercedeFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val newIssuer: String
) extends FuncDataStruct

case class IssueFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved

case class DestroyFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved

case class SplitFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val unity: Long
) extends FuncDataStruct

case class SendFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val recipient: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved

// TO DO: The following case classes need to be verified
case class TransferFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val sender: String,
  val recipient: String,
  override val amount: Long,
) extends FuncDataStruct with FuncAmtInvolved

case class DepositFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val sender: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved

case class WithdrawFuncData(
  override val functionIndex: Int,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val recipient: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved