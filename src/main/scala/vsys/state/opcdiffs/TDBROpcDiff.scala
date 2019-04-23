package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right}

object TDBROpcDiff {

  // for tokenInfo DB
  def get(context: ExecutionContext)(stateVar: Array[Byte], tokenIndex: DataEntry,
                                     dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVar, DataType(stateVar(1)))) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if (tokenIndex.dataType != DataType.Int32) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(GenericError("Out of data range"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenVarKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVar(0))))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else {
        context.state.tokenInfo(tokenVarKey) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Left(GenericError(s"Required variable not defined"))
        }
      }
    }
  }

  // in current version only total store in tokenAccountBalance DB
  def total(context: ExecutionContext)(stateVarTotal: Array[Byte], tokenIndex: DataEntry,
                                       dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVarTotal, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if (tokenIndex.dataType != DataType.Int32) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(GenericError("Out of data range"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0))))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else {
        val t = context.state.tokenAccountBalance(tokenTotalKey)
        Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(t), DataType.Amount)), 1))
      }
    }
  }

  object TDBRType extends Enumeration {
    val GetTDBR = Value(1)
    val TotalTDBR = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = bytes.head match {
    case opcType: Byte if opcType == TDBRType.GetTDBR.id && checkInput(bytes.slice(0, bytes.length - 1), 3, context.stateVar.length, data.length, 2) =>
      get(context)(context.stateVar(bytes(1)), data(bytes(2)), data, bytes(3))
    case opcType: Byte if opcType == TDBRType.TotalTDBR.id && checkInput(bytes.slice(0, bytes.length - 1), 3, context.stateVar.length, data.length, 2) =>
      total(context)(context.stateVar(bytes(1)), data(bytes(2)), data, bytes(3))
    case _ => Left(GenericError("Wrong TDBR opcode"))
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, stateVarLength: Int, dataLength: Int, sep: Int): Boolean = {
    bytes.length == bLength && bytes.slice(1, sep).forall(_ < stateVarLength) && bytes.slice(sep, bLength).forall(_ < dataLength) && bytes.tail.min >= 0
  }

}
