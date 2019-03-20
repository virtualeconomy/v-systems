package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right}

object TDBOpcDiff {

  def newToken(context: ExecutionContext)
              (stateVarMax: Array[Byte], stateVarTotal: Array[Byte], stateVarDesc: Array[Byte],
               max: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarMax, DataType.Amount) || !checkStateVar(stateVarTotal, DataType.Amount)
      || !checkStateVar(stateVarDesc, DataType.ShortText)) {
      Left(GenericError(s"wrong stateVariable"))
    } else if (max.dataType != DataType.Amount) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(max.data) < 0) {
      Left(GenericError("Invalid token max"))
    } else {
      val tokenIndex = context.state.contractTokens(context.contractId.bytes)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, Ints.toByteArray(tokenIndex)))
      Right(OpcDiff(
        tokenDB = Map(
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0)))) -> max.bytes,
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarDesc(0)))) -> context.description),// wrong description
        contractTokens = Map(context.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> 0L)
      ))
    }
  }

  def split(context: ExecutionContext)
             (stateVarUnity: Array[Byte], newUnity: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarUnity, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if (newUnity.dataType != DataType.Amount) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val newUnityValue = Longs.fromByteArray(newUnity.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenUnityKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarUnity(0))))
      if (newUnityValue <= 0) {
        Left(GenericError("Invalid unity value"))
      } else {
        Right(OpcDiff(tokenDB = Map(tokenUnityKey -> newUnity.data)))
      }
    }
  }

  object TDBType extends Enumeration {
    val NewTokenTDB = Value(1)
    val SplitTDB = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TDBType.NewTokenTDB.id && checkInput(bytes, 5, context.stateVar.length, data.length, 4) =>
      newToken(context)(context.stateVar(bytes(1)), context.stateVar(bytes(2)), context.stateVar(bytes(3)), data(bytes(4)))
    case opcType: Byte if opcType == TDBType.SplitTDB.id && checkInput(bytes, 4, context.stateVar.length, data.length, 2) =>
      split(context)(context.stateVar(bytes(1)), data(bytes(2)), data(bytes(3)))
    case _ => Left(GenericError("Wrong TDB opcode"))
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, stateVarLength: Int, dataLength: Int, sep: Int): Boolean = {
    bytes.length == bLength && !bytes.slice(1, sep).isEmpty && !bytes.slice(sep, bLength).isEmpty &&
      bytes.slice(1, sep).max < stateVarLength && bytes.slice(sep, bLength).max < dataLength && bytes.tail.min >= 0
  }

}
