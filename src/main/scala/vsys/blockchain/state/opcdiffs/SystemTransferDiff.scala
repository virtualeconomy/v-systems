package vsys.blockchain.state.opcdiffs

import cats._
import cats.implicits._
import com.google.common.primitives.Longs
import vsys.account.{Account, ContractAccount}
import vsys.blockchain.contract.{CallType, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.{LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}

import scala.util.Left

object SystemTransferDiff {

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {
    lazy val feePortDiff: Map[Account, Portfolio] = toPortDiff(context.signers.head.toAddress, 0)
    lazy val fromAccountPortDiff: Map[Account, Portfolio] = toPortDiff(Account.fromBytes(sender.data, 0).right.get._1, -Longs.fromByteArray(amount.data))
    lazy val toAccountPortDiff: Map[Account, Portfolio] = toPortDiff(Account.fromBytes(recipient.data, 0).right.get._1, Longs.fromByteArray(amount.data))
    lazy val portDiff = Monoid.combineAll(Seq(feePortDiff, fromAccountPortDiff, toAccountPortDiff))

    def fromOpcFuncDiffer(df: OpcDiff, contractId: ContractAccount, callType: CallType.Value, callIndex: Int)
                         (data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = (for {
      exContext <- ExecutionContext.fromCallOpc(context, df, contractId, callType, callIndex)
      diff <- OpcFuncDiffer(exContext)(data)
    } yield diff) match {
      case Right(cdf) => Right(cdf.combine(OpcDiff(portfolios = portDiff)))
      case Left(vd) => Left(vd)}

    def fromOpcFuncDiffer2(df: OpcDiff, contractId: ContractAccount, callType: CallType.Value, callIndex: Int)
                          (contractId2: ContractAccount, callType2: CallType.Value, callIndex2: Int)
                          (data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = (for {
      exContext <- ExecutionContext.fromCallOpc(context, df, contractId, callType, callIndex)
      diff <- OpcFuncDiffer(exContext)(data)
    } yield diff) match {
      case Right(cd) =>
        val combineDiff = cd.combine(df)
        (for {
          exContext <- ExecutionContext.fromCallOpc(context, combineDiff, contractId2, callType2, callIndex2)
          diff <- OpcFuncDiffer(exContext)(data)
        } yield diff) match {
          case Right(cdf) => Right(cdf.combine(cd).combine(OpcDiff(portfolios = portDiff)))
          case Left(vd) => Left(vd)
        }
      case Left(vd) => Left(vd)}

    if (!checkDataType(sender, recipient)) {
      Left(ContractDataTypeMismatch)
    } else {
      sender.dataType match {
        case DataType.Address if context.signers.head.bytes.arr sameElements sender.data =>
          recipient.dataType match {
            case DataType.Address => Right(OpcDiff(portfolios = portDiff))
            case _ => fromOpcFuncDiffer(OpcDiff(portfolios = feePortDiff.combine(fromAccountPortDiff)),
              ContractAccount.fromBytes(recipient.data).right.get, CallType.Trigger, 1)(Seq(sender, recipient, amount))
          }
        case _ => recipient.dataType match {
          case DataType.Address => fromOpcFuncDiffer(OpcDiff(portfolios = feePortDiff),
            ContractAccount.fromBytes(sender.data).right.get, CallType.Trigger, 2)(Seq(sender, recipient, amount))
          case _ => fromOpcFuncDiffer2(OpcDiff(portfolios = feePortDiff), ContractAccount.fromBytes(sender.data).right.get,
            CallType.Trigger, 2)(ContractAccount.fromBytes(recipient.data).right.get, CallType.Trigger, 1)(Seq(sender, recipient, amount))
        }
      }
    }
  }

  object TransferType extends Enumeration {
    val Transfer = Value(1)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TransferType.Transfer.id && checkInput(bytes,4, data.length) =>
      transfer(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case _ => Left(ContractInvalidOPCData)
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, dataLength: Int): Boolean = {
    bytes.length == bLength && bytes.tail.max < dataLength && bytes.tail.min >= 0
  }

  private def checkDataType(sender: DataEntry, recipient: DataEntry): Boolean = {
    (sender.dataType == DataType.Address || sender.dataType == DataType.ContractAccount) &&
      (recipient.dataType == DataType.Address || recipient.dataType == DataType.ContractAccount)
  }

  private def toPortDiff(account: Account,
                         amount: Long): Map[Account, Portfolio] = Map(
    account -> Portfolio(
      balance = amount,
      LeaseInfo.empty,
      assets = Map.empty))

}
