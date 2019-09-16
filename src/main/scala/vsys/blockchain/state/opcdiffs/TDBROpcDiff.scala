package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractInvalidTokenInfo, ContractLocalVariableIndexOutOfRange}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object TDBROpcDiff {

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

  def maxWithoutTokenIndex(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    max(context)(tokenIndex, dataStack, pointer)
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

  def totalWithoutTokenIndex(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    total(context)(tokenIndex, dataStack, pointer)
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

  def unityWithoutTokenIndex(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    unity(context)(tokenIndex, dataStack, pointer)
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

  def descWithoutTokenIndex(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    desc(context)(tokenIndex, dataStack, pointer)
  }

  object TDBRType extends Enumeration(1) {
    val MaxTDBR, TotalTDBR, UnityTDBR, DescTDBR = Value
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    if (checkTDBRODataIndex(bytes.slice(0, bytes.length - 1), data.length)) {
      getTDBRDiff(context)(bytes, data)
    }
    else
      Left(ContractInvalidOPCData)
  }

  private def getTDBRDiff(context: ExecutionContext)
                         (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    (bytes.headOption.flatMap(f => Try(TDBRType(f)).toOption), bytes.length) match {
      case (Some(TDBRType.MaxTDBR), 2) => maxWithoutTokenIndex(context)(data, bytes(1))
      case (Some(TDBRType.MaxTDBR), 3) => max(context)(data(bytes(1)), data, bytes(2))
      case (Some(TDBRType.TotalTDBR), 2) => totalWithoutTokenIndex(context)(data, bytes(1))
      case (Some(TDBRType.TotalTDBR), 3) => total(context)(data(bytes(1)), data, bytes(2))
      case (Some(TDBRType.UnityTDBR), 2) => unityWithoutTokenIndex(context)(data, bytes(1))
      case (Some(TDBRType.UnityTDBR), 3) => unity(context)(data(bytes(1)), data, bytes(2))
      case (Some(TDBRType.DescTDBR), 2) => descWithoutTokenIndex(context)(data, bytes(1))
      case (Some(TDBRType.DescTDBR), 3) => desc(context)(data(bytes(1)), data, bytes(2))
      case _ => Left(ContractInvalidOPCData)
    }
  }

  private def checkTDBRODataIndex(bytes: Array[Byte], dataLength: Int): Boolean =
    bytes.length == 1 || (bytes.tail.max < dataLength && bytes.tail.min >= 0)
}
