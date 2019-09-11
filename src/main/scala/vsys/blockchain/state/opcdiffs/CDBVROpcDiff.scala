package vsys.blockchain.state.opcdiffs

import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractInvalidStateVariable, ContractLocalVariableIndexOutOfRange, ContractStateVariableNotDefined}
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.Contract.checkStateVar

import scala.util.{Left, Right}

object CDBVROpcDiff extends OpcDiffer{

  def get(context: ExecutionContext)(stateVar: Array[Byte], dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVar, DataType(stateVar(1)))) {
      Left(ContractInvalidStateVariable)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))) match {
        case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
        case _ => Left(ContractStateVariableNotDefined)
      }
    }
  }

  object CDBVRType extends Enumeration {
    val GetCDBVR = Value(1)
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption match {
      case Some(opcType: Byte) if opcType == CDBVRType.GetCDBVR.id && bytes.length == 3 && bytes(1) < context.stateVar.length &&
        bytes(1) >= 0 => get(context)(context.stateVar(bytes(1)), data, bytes(2))
      case _ => Left(ContractInvalidOPCData)
    }

}
