package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractInvalidTokenInfo}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object TDBOpcDiff extends OpcDiffer {

  def newToken(context: ExecutionContext)
              (max: DataEntry, unity: DataEntry, desc: DataEntry): Either[ValidationError, OpcDiff] = {

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
           (newUnity: DataEntry, tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {

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

  object TDBType extends Enumeration {
    sealed case class TDBTypeVal(
      tdbType: Int,
      operandCount: Int,
      withTokenIndex: Boolean,
      differ: (ExecutionContext, Array[Byte], Seq[DataEntry]) => Either[ValidationError, OpcDiff])
    extends Val(tdbType) { def *(n: Int): Int = n * tdbType }

    val NewTokenTDB = TDBTypeVal(1, 3, false, (c, b, d) => newToken(c)(d(b(1)), d(b(2)), d(b(3))))
    val SplitTDB    = TDBTypeVal(2, 2, true,  (c, b, d) => split(c)(d(b(1)), tokenIndex(b, d, 2)))

    def fromByte(b: Byte): Option[TDBType.TDBTypeVal] = Try(TDBType(b).asInstanceOf[TDBTypeVal]).toOption
  }

  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    bytes.headOption.flatMap(TDBType.fromByte(_)) match {
      case Some(t: TDBType.TDBTypeVal) if checkData(bytes, data.length, t.operandCount, t.withTokenIndex) => t.differ(context, bytes, data)
      case _ => Left(ContractInvalidOPCData)
    }
}
