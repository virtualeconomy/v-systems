package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right}

object LoadOpcDiff {

  def signer(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(GenericError("Out of data range"))
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(context.signers.head.bytes.arr, DataType.Address)), 1))
    }
  }

  def caller(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(GenericError("Out of data range"))
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(context.signers.head.bytes.arr, DataType.Address)), 1))
    }
  }

  object LoadType extends Enumeration {
    val SignerLoad = Value(1)
    val CallerLoad = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == LoadType.SignerLoad.id && bytes.length == 2 => signer(context)(data, bytes.last)
    case opcType: Byte if opcType == LoadType.CallerLoad.id && bytes.length == 2 => caller(context)(data, bytes.last)
    case _ => Left(GenericError("Wrong Load opcode"))
  }

}
