package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.opcdiffs.OpcDiff
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.ContractInvalidOPCData

import scala.util.Left

object SystemTransferDiff {

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {
    Right(OpcDiff.empty)
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
}