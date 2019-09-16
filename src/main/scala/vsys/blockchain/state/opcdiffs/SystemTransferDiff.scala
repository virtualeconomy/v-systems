package vsys.blockchain.state.opcdiffs

import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}

import scala.util.Left

object SystemTransferDiff {

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkDataType(sender, recipient)) {
      Left(ContractDataTypeMismatch)
    } else {
      sender.dataType match {
        case DataType.Address if context.signers.head.toAddress.bytes.arr sameElements sender.bytes =>
          recipient.dataType match {
            case DataType.Address => Right(OpcDiff.empty)
            case _ => Right(OpcDiff.empty)
          }
        case _ => recipient.dataType match {
          case DataType.Address => Right(OpcDiff.empty)
          case _ => Right(OpcDiff.empty)
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
      (recipient.dataType == DataType.Address && recipient.dataType == DataType.ContractAccount)
  }
}
