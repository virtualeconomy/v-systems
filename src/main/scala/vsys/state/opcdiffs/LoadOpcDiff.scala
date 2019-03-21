package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right}

object LoadOpcDiff {

  def signer(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    Right(dataStack :+ DataEntry(context.signers.head.bytes.arr, DataType.Address))
  }

  def caller(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    Right(dataStack :+ DataEntry(context.signers.head.bytes.arr, DataType.Address))
  }

  object LoadType extends Enumeration {
    val SignerLoad = Value(1)
    val CallerLoad = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == LoadType.SignerLoad.id && bytes.length == 1 => signer(context)(data)
    case opcType: Byte if opcType == LoadType.CallerLoad.id && bytes.length == 1 => caller(context)(data)
    case _ => Left(GenericError("Wrong Load opcode"))
  }

}
