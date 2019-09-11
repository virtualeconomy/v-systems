package vsys.blockchain.state.opcdiffs

import cats.implicits._
import com.google.common.primitives.Shorts
import vsys.blockchain.state.reader.CompositeStateReader
import vsys.utils.serialization.Deser
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidFunction, ContractInvalidOPCData}
import vsys.utils.ScorexLogging
import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.contract.DataType.checkTypes

import scala.util.{Failure, Success, Try}


object OpcFuncDiffer extends ScorexLogging {

  def apply(executionContext: ExecutionContext)
           (data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    val opcFunc = executionContext.opcFunc
    val height = executionContext.height
    val tx = executionContext.transaction
    val s = executionContext.state
    fromBytes(opcFunc) match {
      case Success((_, _, _, listParaTypes, listOpcLines)) =>
        for {
          _ <- Either.cond(
                 checkTypes(listParaTypes, data.map(_.dataType.id.toByte).toArray),
                 (), ContractDataTypeMismatch
               )
          _ <- Either.cond(
                 listOpcLines.forall(_.length >= 2),
                 (), ContractInvalidOPCData
               )
          diff <- 
            listOpcLines.foldLeft(Right((OpcDiff.empty, data))) {
              case (ei, opc) =>
                ei.flatMap((processedDiff, oldData) => {
                  val processedState = new CompositeStateReader(s, processedDiff.asBlockDiff(height, tx))
                  OpcDiffer(executionContext.copy(state = processedState))(opc, oldData) map {
                    (opcDiff, newData) => (processedDiff.combine(opcDiff), newData)
                  }
                })
            } map { (functionDiff, _) => functionDiff }
        } yield diff
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
