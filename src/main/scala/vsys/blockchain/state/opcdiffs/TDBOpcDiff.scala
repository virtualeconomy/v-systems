package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractInvalidTokenInfo}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.ExecutionContext

import scala.util.{Left, Right}

object TDBOpcDiff {

  def newToken(context: ExecutionContext)
              (max: DataEntry, unity: DataEntry, desc: DataEntry):Either[ValidationError, OpcDiff] = {

    if (max.dataType != DataType.Amount || unity.dataType != DataType.Amount || desc.dataType != DataType.ShortText) {
      Left(ContractDataTypeMismatch)
    } else if (Longs.fromByteArray(max.data) < 0) {
      Left(ContractInvalidTokenInfo)
    } else if (Longs.fromByteArray(unity.data) <= 0) {
      Left(ContractInvalidTokenInfo)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, Ints.toByteArray(contractTokens)).right.get
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
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val newUnityValue = Longs.fromByteArray(newUnity.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenUnityKey = ByteStr(Bytes.concat(tokenID.arr, Array(2.toByte)))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else if (newUnityValue <= 0) {
        Left(ContractInvalidTokenInfo)
      } else {
        Right(OpcDiff(tokenDB = Map(tokenUnityKey -> newUnity.bytes)))
      }
    }
  }

  def splitWithoutTokenIndex(context: ExecutionContext)
           (newUnity: DataEntry): Either[ValidationError, OpcDiff] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    split(context)(newUnity, tokenIndex)
  }

  object TDBType extends Enumeration {
    val NewTokenTDB = Value(1)
    val SplitTDB = Value(2)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    if (checkTDBDataIndex(bytes, data.length)) {
      val newTokenTDBId = TDBType.NewTokenTDB.id.toByte
      val splitTDBId = TDBType.SplitTDB.id.toByte
      val NewTokenDataLength = 4
      (bytes.head, bytes.length) match {
        case (`newTokenTDBId`, `NewTokenDataLength`) => newToken(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
        case (`splitTDBId`, 2) => splitWithoutTokenIndex(context)(data(bytes(1)))
        case (`splitTDBId`, 3) => split(context)(data(bytes(1)), data(bytes(2)))
        case _ => Left(ContractInvalidOPCData)
      }
    }
    else
      Left(ContractInvalidOPCData)
  }

  private def checkTDBDataIndex(bytes: Array[Byte], dataLength: Int): Boolean =
    bytes.tail.max < dataLength && bytes.tail.min >= 0

}
