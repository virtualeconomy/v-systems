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
              (stateVarMax: Array[Byte], stateVarTotal: Array[Byte], stateVarUnity: Array[Byte],
               stateVarDesc: Array[Byte], max: DataEntry, unity: DataEntry, desc: DataEntry):Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarMax, DataType.Amount) || !checkStateVar(stateVarTotal, DataType.Amount)
      || !checkStateVar(stateVarUnity, DataType.Amount) || !checkStateVar(stateVarDesc, DataType.ShortText)) {
      Left(GenericError("Wrong stateVariable"))
    } else if (max.dataType != DataType.Amount || unity.dataType != DataType.Amount || desc.dataType != DataType.ShortText) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(max.data) < 0) {
      Left(GenericError(s"Invalid token max ${Longs.fromByteArray(max.data)}"))
    } else if (Longs.fromByteArray(unity.data) <= 0) {
      Left(GenericError(s"Invalid token unity ${Longs.fromByteArray(unity.data)}"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, Ints.toByteArray(contractTokens)))
      Right(OpcDiff(
        tokenDB = Map(
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0)))) -> max.bytes,
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarUnity(0)))) -> unity.bytes,
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarDesc(0)))) -> desc.bytes),
        contractTokens = Map(context.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> 0L)
      ))
    }
  }

  def split(context: ExecutionContext)
           (stateVarUnity: Array[Byte], newUnity: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarUnity, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if (newUnity.dataType != DataType.Amount || tokenIndex.dataType != DataType.Int32) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val newUnityValue = Longs.fromByteArray(newUnity.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenUnityKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarUnity(0))))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else if (newUnityValue <= 0) {
        Left(GenericError(s"Invalid unity value $newUnityValue"))
      } else {
        Right(OpcDiff(tokenDB = Map(tokenUnityKey -> newUnity.bytes)))
      }
    }
  }

  object TDBType extends Enumeration {
    val NewTokenTDB = Value(1)
    val SplitTDB = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TDBType.NewTokenTDB.id && checkInput(bytes, 8, context.stateVar.length, data.length, 5) =>
      newToken(context)(context.stateVar(bytes(1)), context.stateVar(bytes(2)), context.stateVar(bytes(3)), context.stateVar(bytes(4)), data(bytes(5)), data(bytes(6)), data(bytes(7)))
    case opcType: Byte if opcType == TDBType.SplitTDB.id && checkInput(bytes, 4, context.stateVar.length, data.length, 2) =>
      split(context)(context.stateVar(bytes(1)), data(bytes(2)), data(bytes(3)))
    case _ => Left(GenericError("Wrong TDB opcode"))
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, stateVarLength: Int, dataLength: Int, sep: Int): Boolean = {
    bytes.length == bLength && bytes.slice(1, sep).forall(_ < stateVarLength) && bytes.slice(sep, bLength).forall(_ < dataLength) && bytes.tail.min >= 0
  }

}
