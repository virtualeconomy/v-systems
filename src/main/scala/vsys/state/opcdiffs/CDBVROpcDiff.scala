package vsys.state.opcdiffs

import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType, ExecutionContext}
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right}

object CDBVROpcDiff {

  def get(context: ExecutionContext)(stateVar: Array[Byte],
                                     dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVar, DataType(stateVar(1)))) {
      Left(GenericError(s"Wrong stateVariable"))
    } else {
      context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))) match {
        case Some(v) => Right(dataStack :+ v)
        case _ => Left(GenericError(s"Required variable not defined"))
      }
    }
  }

  object CDBVRType extends Enumeration {
    val GetCDBVR = Value(1)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == CDBVRType.GetCDBVR.id && bytes.length == 2 && bytes.tail.max < context.stateVar.length &&
      bytes.tail.min >= 0 => get(context)(context.stateVar(bytes(1)), data)
    case _ => Left(GenericError("Wrong CDBVR opcode"))
  }

}
