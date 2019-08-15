package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractUnsupportedOPC}
import vsys.blockchain.contract.{DataEntry, ExecutionContext}

import scala.util.Left

object ReturnOpcDiff {

  def value(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    Left(ContractUnsupportedOPC)
  }

  object ReturnType extends Enumeration {
    val ValueReturn = Value(1)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == ReturnType.ValueReturn.id && bytes.length == 2 => value(context)(data, bytes.last)
    case _ => Left(ContractInvalidOPCData)
  }

}
