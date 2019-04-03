package vsys.state.opcdiffs

import cats.implicits._
import com.google.common.primitives.Shorts
import com.wavesplatform.state2.reader.CompositeStateReader
import scorex.serialization.Deser
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.utils.ScorexLogging
import vsys.contract.{DataEntry, ExecutionContext}
import vsys.contract.DataType.checkTypes

import scala.util.{Failure, Success, Try}


object OpcFuncDiffer extends ScorexLogging {

  def right(structure: (OpcDiff, Seq[DataEntry])): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = Right(structure)

  def apply(executionContext: ExecutionContext)
           (data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    val opcFunc = executionContext.opcFunc
    val height = executionContext.height
    val tx = executionContext.transaction
    val s = executionContext.state
    fromBytes(opcFunc) match {
      case Success((_, _, listParaTypes, listOpcLines)) =>
        if (!checkTypes(listParaTypes, data.map(_.dataType.id.toByte).toArray)) {
          Left(ValidationError.InvalidDataEntry)
        } else if (listOpcLines.forall(_.length < 2)) {
          Left(ValidationError.InvalidContract)
        } else {
          listOpcLines.foldLeft(right((OpcDiff.empty, data))) { case (ei, opc) => ei.flatMap(st =>
            OpcDiffer(executionContext.copy(state = new CompositeStateReader(s,
              st._1.asBlockDiff(height, tx))))(opc, st._2) match {
              case Right((opcDiff, d)) => Right((st._1.combine(opcDiff), d))
              case Left(l) => Left(l)
            }
          )} match {
            case Right((opcDiff, _)) => Right(opcDiff)
            case Left(l) => Left(l)
          }
        }
      case Failure(_) => Left(GenericError("Invalid opc function"))
    }
  }

  private def fromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Seq[Array[Byte]])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val (protoTypeBytes, protoTypeEnd) = Deser.parseArraySize(bytes, 2)
    val returnType = protoTypeBytes.head
    val listParaTypes = protoTypeBytes.tail
    val (listOpcLinesBytes, _) = Deser.parseArraySize(bytes, protoTypeEnd)
    val listOpcLines = Deser.parseArrays(listOpcLinesBytes)
    (funcIdx, returnType, listParaTypes, listOpcLines)
  }

}
