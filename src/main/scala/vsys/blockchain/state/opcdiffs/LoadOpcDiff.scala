package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractLocalVariableIndexOutOfRange}

import scala.util.{Left, Right, Try}

object LoadOpcDiff extends OpcDiffer {

  def signer(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(context.signers.head.bytes.arr, DataType.Address)), 1))
    }
  }

  def caller(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    signer(context)(dataStack, pointer)
  }

  def timestamp(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(context.prevBlockTimestamp.getOrElse(0L)), DataType.Timestamp)), 1))
    }
  }

  def lastTokenIndex(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      Right(dataStack.patch(pointer, Seq(DataEntry(Ints.toByteArray(contractTokens - 1), DataType.Int32)), 1))
    }
  }

  def transactionId(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val txId = context.transaction.id
      Right(dataStack.patch(pointer, Seq(DataEntry(txId.arr, DataType.ShortText)), 1))
    }
  }

  def signerPublicKey(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(context.signers.head.publicKey, DataType.PublicKey)), 1))
    }
  }

  def height(context: ExecutionContext)(dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(context.height), DataType.Int32)), 1))
    }
  }

  object LoadType extends Enumeration(1) {
    val SignerLoad, CallerLoad, TimestampLoad, LastTokenIndexLoad, TransactionIdLoad, SignerPublicKeyLoad = Value
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    (bytes.headOption.flatMap(b => Try(LoadType(b)).toOption), bytes.length) match {
      case (Some(LoadType.SignerLoad), 2) => signer(context)(data, bytes.last)
      case (Some(LoadType.CallerLoad), 2) => caller(context)(data, bytes.last)
      case (Some(LoadType.TimestampLoad), 2) => timestamp(context)(data, bytes.last)
      case (Some(LoadType.LastTokenIndexLoad), 2) => lastTokenIndex(context)(data, bytes.last)
      case (Some(LoadType.TransactionIdLoad), 2) => transactionId(context)(data, bytes.last)
      case (Some(LoadType.SignerPublicKeyLoad), 2) => signerPublicKey(context)(data, bytes.last)
      case _ => Left(ContractInvalidOPCData)
    }
}
