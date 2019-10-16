package vsys.blockchain.state.systemdiffs

import cats.implicits._
import com.google.common.primitives.Shorts
import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.contract.DataType.checkTypes
import vsys.blockchain.state.opcdiffs.OpcDiff
import vsys.blockchain.state.reader.CompositeStateReader
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidFunction, ContractInvalidOPCData}
import vsys.utils.serialization.Deser

import scala.util.{Failure, Success, Try}

object SystemFuncDiffer {

  def right(structure: (OpcDiff, Seq[DataEntry])): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = Right(structure)

  def apply(executionContext: ExecutionContext)
           (data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    val opcFunc = executionContext.opcFunc
    val height = executionContext.height
    val tx = executionContext.transaction
    val s = executionContext.state
    fromBytes(opcFunc) match {
      case Success((_, _, _, listParaTypes, listOpcLines)) =>
        if (!checkTypes(listParaTypes, data.map(_.dataType.id.toByte).toArray)) {
          Left(ContractDataTypeMismatch)
        } else if (listOpcLines.forall(_.length < 2)) {
          Left(ContractInvalidOPCData)
        } else {
          listOpcLines.foldLeft(right((OpcDiff.empty, data))) { case (ei, opc) => ei.flatMap(st =>
            SystemDiffer(executionContext.copy(state = new CompositeStateReader(s,
              st._1.asBlockDiff(height, tx))))(opc, st._2) match {
              case Right((opcDiff, d)) => Right((st._1.combine(opcDiff), d))
              case Left(l) => Left(l)
            }
          )} match {
            case Right((opcDiff, _)) => Right(opcDiff)
            case Left(l) => Left(l)
          }
        }
      case Failure(_) => Left(ContractInvalidFunction)
    }
  }

  private def fromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Array[Byte], Seq[Array[Byte]])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val funcType = bytes(2)
    val (listReturnTypes, listReturnTypeEnd) = Deser.parseArraySize(bytes, 3)
    val (listParaTypes, listParaTypeEnd) = Deser.parseArraySize(bytes, listReturnTypeEnd)
    val (listOpcLinesBytes, _) = Deser.parseArraySize(bytes, listParaTypeEnd)
    val listOpcLines = Deser.parseArrays(listOpcLinesBytes)
    (funcIdx, funcType,listReturnTypes, listParaTypes, listOpcLines)
  }

}
