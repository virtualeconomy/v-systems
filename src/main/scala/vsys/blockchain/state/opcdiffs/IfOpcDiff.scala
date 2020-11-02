package vsys.blockchain.state.opcdiffs

import cats.implicits._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.reader.CompositeStateReader
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.utils.serialization.Deser

import scala.util.{Left, Right, Try, Failure, Success}

object IfOpcDiff extends OpcDiffer {

  def executeOpcBlock(context: ExecutionContext, opcBlock: DataEntry, dataStack: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = {
    opcBlock.dataType match {
      case DataType.OpcBlock => {
        val opcBytes = DataType.OpcBlock.deserializer(opcBlock.data)
        if(opcBytes.length == 0) Right((OpcDiff.empty, dataStack))
        else Try(Deser.parseArrays(opcBytes)) match {
          case Success(opclines) => {
            opclines.foldLeft(Right((OpcDiff.empty, dataStack)): Either[ValidationError, (OpcDiff, Seq[DataEntry])]) {
              case (acc, opc) => acc.flatMap { case (oldDiff, oldData) => {
                val oldState = new CompositeStateReader(context.state, oldDiff.asBlockDiff(context.height, context.transaction))
                OpcDiffer(context.copy(state = oldState))(opc, oldData) map { case (opcDiff, newData) => (oldDiff.combine(opcDiff), newData) }
              }}
            }
          }
          case Failure(_) => Left(ContractInvalidOPCData)
        }
      }
      case _ => Left(ContractDataTypeMismatch)
    }
  }

  def runCondition(cond: DataEntry): Either[ValidationError, Boolean] = {
    cond.dataType match {
      case DataType.Boolean => Right(cond.data sameElements Array(1.toByte))
      case _ => Left(ContractDataTypeMismatch)
    }
  }

  object IfType extends Enumeration(1) {
    val If, IfElse = Value
  }

  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    throw new UnsupportedOperationException(s"Could not be reached method")

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    throw new UnsupportedOperationException(s"Could not be reached method")

  override def parseBytes(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] =
    (bytes.headOption.flatMap(f => Try(IfType(f)).toOption), bytes.length) match {
      case (Some(IfType.If), 3) if checkData(bytes, data.length, 2, false) => {
        runCondition(data(bytes(1))).flatMap(cond =>
          if(cond) executeOpcBlock(context, data(bytes(2)), data)
          else Right((OpcDiff.empty, data))
        )
      }
     case (Some(IfType.IfElse), 4) if checkData(bytes, data.length, 3, false) => {
        runCondition(data(bytes(1))).flatMap(cond =>
          if(cond) executeOpcBlock(context, data(bytes(2)), data)
          else executeOpcBlock(context, data(bytes(3)), data)
        )
      }
    }
}
