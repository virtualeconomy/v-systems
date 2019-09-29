package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractUnsupportedOPC}
import vsys.blockchain.contract.{DataEntry, ExecutionContext}

import scala.util.{Left, Try}

object ReturnOpcDiff extends OpcDiffer {

  def value(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] =
    Left(ContractUnsupportedOPC)

  object ReturnType extends Enumeration {
    val ValueReturn = Value(1)
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(ReturnType(f)).toOption) match {
      case Some(ReturnType.ValueReturn) if bytes.length == 2 => value(context)(data, bytes.last)
      case _ => Left(ContractInvalidOPCData)
    }
}
