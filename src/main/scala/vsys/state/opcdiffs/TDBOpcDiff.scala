package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext

import scala.util.{Left, Right}

object TDBOpcDiff {

  def newToken(context: ExecutionContext)
              (max: DataEntry, unity: DataEntry, desc: DataEntry):Either[ValidationError, OpcDiff] = {

    if (max.dataType != DataType.Amount || unity.dataType != DataType.Amount || desc.dataType != DataType.ShortText) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(max.data) < 0) {
      Left(GenericError(s"Invalid token max ${Longs.fromByteArray(max.data)}"))
    } else if (Longs.fromByteArray(unity.data) <= 0) {
      Left(GenericError(s"Invalid token unity ${Longs.fromByteArray(unity.data)}"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, Ints.toByteArray(contractTokens)))
      val tokenMaxKey = Bytes.concat(tokenID.arr, Array(0.toByte))
      val tokenTotalKey = Bytes.concat(tokenID.arr, Array(1.toByte))
      val tokenUnityKey = Bytes.concat(tokenID.arr, Array(2.toByte))
      val tokenDescKey = Bytes.concat(tokenID.arr, Array(3.toByte))
      Right(OpcDiff(
        tokenDB = Map(
          ByteStr(tokenMaxKey) -> max.bytes,
          ByteStr(tokenUnityKey) -> unity.bytes,
          ByteStr(tokenDescKey) -> desc.bytes),
        contractTokens = Map(context.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(tokenTotalKey) -> 0L)
      ))
    }
  }

  def split(context: ExecutionContext)
           (newUnity: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (newUnity.dataType != DataType.Amount || tokenIndex.dataType != DataType.Int32) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val newUnityValue = Longs.fromByteArray(newUnity.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenUnityKey = ByteStr(Bytes.concat(tokenID.arr, Array(2.toByte)))
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
    case opcType: Byte if opcType == TDBType.NewTokenTDB.id && checkInput(bytes,4, data.length) =>
      newToken(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case opcType: Byte if opcType == TDBType.SplitTDB.id && checkInput(bytes,3, data.length) =>
      split(context)(data(bytes(1)), data(bytes(2)))
    case _ => Left(GenericError("Wrong TDB opcode"))
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, dataLength: Int): Boolean = {
    bytes.length == bLength && bytes.tail.max < dataLength && bytes.tail.min >= 0
  }

}
