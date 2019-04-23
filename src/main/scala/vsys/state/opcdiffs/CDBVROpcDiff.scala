package vsys.state.opcdiffs

import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType, ExecutionContext}
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right}

object CDBVROpcDiff {

  def get(context: ExecutionContext)(stateVar: Array[Byte], dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVar, DataType(stateVar(1)))) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(GenericError(s"Out of data range"))
    } else {
      context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))) match {
        case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
        case _ => Left(GenericError(s"Required variable not defined"))
      }
    }
  }

  object CDBVRType extends Enumeration {
    val GetCDBVR = Value(1)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == CDBVRType.GetCDBVR.id && bytes.length == 3 && bytes(1) < context.stateVar.length &&
      bytes(1) >= 0 => get(context)(context.stateVar(bytes(1)), data, bytes(2))
    case _ => Left(GenericError("Wrong CDBVR opcode"))
  }

}
