package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractInvalidTokenInfo, ContractLocalVariableIndexOutOfRange}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object TDBROpcDiff extends OpcDiffer {

  def max(context: ExecutionContext)(tokenIndex: DataEntry,
                                     dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    if (tokenIndex.dataType != DataType.Int32) {
      Left(ContractDataTypeMismatch)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenMaxKey = ByteStr(Bytes.concat(tokenID.arr, Array(0.toByte)))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else {
        context.state.tokenInfo(tokenMaxKey) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Left(ContractInvalidTokenInfo)
        }
      }
    }
  }

  // in current version only total store in tokenAccountBalance DB
  def total(context: ExecutionContext)(tokenIndex: DataEntry,
                                       dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    if (tokenIndex.dataType != DataType.Int32) {
      Left(ContractDataTypeMismatch)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(1.toByte)))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else {
        val t = context.state.tokenAccountBalance(tokenTotalKey)
        Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(t), DataType.Amount)), 1))
      }
    }
  }

  def unity(context: ExecutionContext)(tokenIndex: DataEntry,
                                       dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    if (tokenIndex.dataType != DataType.Int32) {
      Left(ContractDataTypeMismatch)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenUnityKey = ByteStr(Bytes.concat(tokenID.arr, Array(2.toByte)))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else {
        context.state.tokenInfo(tokenUnityKey) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Left(ContractInvalidTokenInfo)
        }
      }
    }
  }

  def desc(context: ExecutionContext)(tokenIndex: DataEntry,
                                      dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    if (tokenIndex.dataType != DataType.Int32) {
      Left(ContractDataTypeMismatch)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenDescKey = ByteStr(Bytes.concat(tokenID.arr, Array(3.toByte)))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else {
        context.state.tokenInfo(tokenDescKey) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Left(ContractInvalidTokenInfo)
        }
      }
    }
  }

  object TDBRType extends Enumeration {
    sealed case class TDBRTypeVal(
      tdbrType: Int,
      operandCount: Int,
      differ: (ExecutionContext, Array[Byte], Seq[DataEntry]) => Either[ValidationError, Seq[DataEntry]])
    extends Val(tdbrType) { def *(n: Int): Int = n * tdbrType }

    private def makeParams(bytes: Array[Byte], data: Seq[DataEntry], operandCount: Int): (DataEntry, Seq[DataEntry], Byte) =
      bytes.tail.length match {
        case `operandCount` => (data(bytes(1)),    data, bytes(2))
        case _            => (defaultTokenIndex, data, bytes(1))
      }

    val MaxTDBR   = TDBRTypeVal(1, 2, (c, b, d) => (max  (c) _).tupled(makeParams(b, d, 2)))
    val TotalTDBR = TDBRTypeVal(2, 2, (c, b, d) => (total(c) _).tupled(makeParams(b, d, 2)))
    val UnityTDBR = TDBRTypeVal(3, 2, (c, b, d) => (unity(c) _).tupled(makeParams(b, d, 2)))
    val DescTDBR  = TDBRTypeVal(4, 2, (c, b, d) => (desc (c) _).tupled(makeParams(b, d, 2)))

    def fromByte(b: Byte): Option[TDBRType.TDBRTypeVal] = Try(TDBRType(b).asInstanceOf[TDBRTypeVal]).toOption
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(TDBRType.fromByte(_)) match {
      case Some(t: TDBRType.TDBRTypeVal) if checkData(bytes.dropRight(1), data.length, t.operandCount - 1) => t.differ(context, bytes, data)
      case _ => Left(ContractInvalidOPCData)
    }

}
