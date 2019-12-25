package vsys.blockchain.contract

trait FuncDataStruct {
  val functionIndex: Int
  val funcName: String
  val signer: String
  val contractId: String
  val tokenId: String
  val transactionFee: Long
}

trait FuncAmtInvolved {
  val amount: Long
}

object FuncDataStruct {}
object FuncAmtInvolved {}

case class SupercedeFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val newIssuer: String
) extends FuncDataStruct {
  override val funcName = "supercede"
}

case class IssueFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "issue"
}

case class DestroyFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "destroy"
}

case class SplitFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val unity: Long
) extends FuncDataStruct {
  override val funcName = "split"
}

case class SendFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val recipient: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "send"
}

// TO DO: The following case classes need to be verified
case class TransferFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val sender: String,
  val recipient: String,
  override val amount: Long,
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "transfer"
}

case class DepositFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val sender: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "deposit"
}

case class WithdrawFuncData(
  override val functionIndex: Int,
  override val transactionFee: Long,
  override val signer: String,
  override val contractId: String,
  override val tokenId: String,
  val recipient: String,
  override val amount: Long
) extends FuncDataStruct with FuncAmtInvolved {
  override val funcName = "withdraw"
}