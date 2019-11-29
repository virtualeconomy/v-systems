package vsys.blockchain.state.opcdiffs

import cats.Monoid
import cats.implicits._
import com.google.common.primitives.{Ints, Longs}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account._
import vsys.blockchain.contract.{CallType, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._

import scala.util.{Left, Right, Try}

object SystemTransferDiff extends OpcDiffer {

  def getTriggerCallOpcDiff(context: ExecutionContext, diff: OpcDiff,
                            sender: DataEntry, recipient: DataEntry, amount: DataEntry, tokenId: DataEntry,
                            callType: CallType.Value, callIndex: Int): Either[ValidationError, OpcDiff] = {
    if (callType == CallType.Trigger){
      callIndex match {
        case 1 =>
          if (sender.dataType == DataType.Address) Right(OpcDiff.empty)
          else {
            val senderContractId = ContractAccount.fromBytes(sender.data).explicitGet()
            CallOpcDiff(context, diff, senderContractId, Seq(recipient, amount, tokenId), callType, callIndex)
          }
        case 2 =>
          if (recipient.dataType == DataType.Address) Right(OpcDiff.empty)
          else {
            val recipientContractId = ContractAccount.fromBytes(recipient.data).explicitGet()
            CallOpcDiff(context, diff, recipientContractId, Seq(sender, amount, tokenId), callType, callIndex)
          }
        case _ => Left(GenericError("Invalid Call Index"))
      }
    } else {
      Left(GenericError("Invalid Call Type"))
    }
  }

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {
    val dType = Array(amount.dataType.id.toByte, sender.dataType.id.toByte, recipient.dataType.id.toByte)
    val rType = Array(DataType.Amount.id.toByte, DataType.Account.id.toByte, DataType.Account.id.toByte)
    val sysTokenId = DataEntry(tokenIdFromBytes(context.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId)
    for {
      _ <- Either.cond(DataType.checkTypes(dType, rType), (), ContractDataTypeMismatch)
      transferAmount = Longs.fromByteArray(amount.data)
      senderAddr <- Account.fromBytes(sender.data, 0)
      recipientAddr <- Account.fromBytes(recipient.data, 0)
      senderBalance = context.state.balance(senderAddr._1)
      recipientBalance = context.state.balance(recipientAddr._1)
      _ <- Either.cond(senderBalance >= transferAmount, (), ContractTokenBalanceInsufficient)
      _ <- Either.cond(Try(Math.addExact(transferAmount, recipientBalance)).isSuccess, (), OverflowError)
      _ <- Either.cond(transferAmount >= 0, (), ContractInvalidAmount)
      senderCallDiff <- getTriggerCallOpcDiff(context, OpcDiff.empty, sender, recipient, amount, sysTokenId, CallType.Trigger, 1)
      senderRelatedAddress = if (sender.dataType == DataType.Address) Map(Address.fromBytes(sender.data).explicitGet() -> true) else Map[Address, Boolean]()
      senderPortDiff: Map[Account, Portfolio] = Map(
        senderAddr._1 -> Portfolio(
          balance = -transferAmount,
          LeaseInfo.empty,
          assets = Map.empty))
      senderDiff = OpcDiff(relatedAddress = senderRelatedAddress, portfolios = senderPortDiff)
      senderTotalDiff = OpcDiff.opcDiffMonoid.combine(senderCallDiff, senderDiff)
      recipientCallDiff <- getTriggerCallOpcDiff(context, senderTotalDiff, sender, recipient, amount, sysTokenId, CallType.Trigger, 2)
      recipientRelatedAddress = if (recipient.dataType == DataType.Address) Map(Address.fromBytes(recipient.data).explicitGet() -> true) else Map[Address, Boolean]()
      recipientPortDiff: Map[Account, Portfolio] = Map(
        recipientAddr._1 -> Portfolio(
          balance = transferAmount,
          LeaseInfo.empty,
          assets = Map.empty))
      recipientDiff = OpcDiff(relatedAddress = recipientRelatedAddress, portfolios = recipientPortDiff)
      returnDiff = Monoid.combineAll(Seq(senderTotalDiff, recipientCallDiff, recipientDiff))
    } yield returnDiff
  }

  object TransferType extends Enumeration {
    val Transfer = Value(1)
  }

  override def parseBytesDf(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TransferType.Transfer.id && checkInput(bytes,4, data.length) =>
      transfer(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case _ => Left(ContractInvalidOPCData)
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, dataLength: Int): Boolean = {
    bytes.length == bLength && bytes.tail.max < dataLength && bytes.tail.min >= 0
  }

}
