package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractInvalidTokenInfo, ContractLocalVariableIndexOutOfRange}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.ExecutionContext

import scala.util.{Left, Right}

object TDBROpcDiff extends OpcDiffer{

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

  object TDBRType extends Enumeration {
    val MaxTDBR = Value(1)
    val TotalTDBR = Value(2)
    val UnityTDBR = Value(3)
    val DescTDBR = Value(4)
    def fromByte(implicit b: Byte): Option[TDBRType.Value] = Try(TDBRType(b)).toOption
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(TDBRType.fromByte(_)) match {
      case Some(TDBRType.MaxTDBR) && checkInput(bytes.slice(0, bytes.length - 1),1, data.length) =>
        maxWithoutTokenIndex(context)(data, bytes(1))
      case Some(TDBRType.MaxTDBR) && checkInput(bytes.slice(0, bytes.length - 1),2, data.length) =>
        max(context)(data(bytes(1)), data, bytes(2))
      case Some(TDBRType.TotalTDBR) && checkInput(bytes.slice(0, bytes.length - 1),1, data.length) =>
        totalWithoutTokenIndex(context)(data, bytes(1))
      case Some(TDBRType.TotalTDBR) && checkInput(bytes.slice(0, bytes.length - 1),2, data.length) =>
        total(context)(data(bytes(1)), data, bytes(2))
      case Some(TDBRType.UnityTDBR) && checkInput(bytes.slice(0, bytes.length - 1),1, data.length) =>
        unityWithoutTokenIndex(context)(data, bytes(1))
      case Some(TDBRType.UnityTDBR) && checkInput(bytes.slice(0, bytes.length - 1),2, data.length) =>
        unity(context)(data(bytes(1)), data, bytes(2))
      case Some(TDBRType.DescTDBR) && checkInput(bytes.slice(0, bytes.length - 1),1, data.length) =>
        descWithoutTokenIndex(context)(data, bytes(1))
      case Some(TDBRType.DescTDBR) && checkInput(bytes.slice(0, bytes.length - 1),2, data.length) =>
        desc(context)(data(bytes(1)), data, bytes(2))
      case _ => Left(ContractInvalidOPCData)
    }

  private def checkInput(bytes: Array[Byte], bLength: Int, dataLength: Int): Boolean = {
    bytes.length == bLength && (bytes.length == 1 || (bytes.tail.max < dataLength && bytes.tail.min >= 0))
  }

}
