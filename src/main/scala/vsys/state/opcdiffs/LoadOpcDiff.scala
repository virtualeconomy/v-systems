package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.{GenericError, InvalidDataEntry}
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.Right

object LoadOpcDiff {

  def issuer(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    context.state.contractInfo(context.contractId.bytes) match {
      case Some(i) => Right(dataStack :+ i)
      case _ => Left(GenericError(s"${context.contractId.address}'s issuer not defined"))
    }
  }

  def sender(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val sender = context.signers.head
    val newDataStack = dataStack :+ DataEntry(sender.bytes.arr, DataType.Address)
    Right(newDataStack)
  }

  def max(context: ExecutionContext)(stateVar: Array[Byte],
                                     tokenIdx: DataEntry,
                                     dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array(stateVar(0))))
    context.state.tokenInfo(id) match {
      case Some(m) if m.dataType == DataType.fromByte(stateVar(1)).get => Right(dataStack :+ m)
      case _ => Left(InvalidDataEntry)
    }

  }

  def total(context: ExecutionContext)(stateVar: Array[Byte],
                                       tokenIdx: DataEntry,
                                       dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array(stateVar(0))))
    val total = context.state.tokenAccountBalance(id).getOrElse(0L)
    val newDataStack = dataStack :+ DataEntry(Longs.toByteArray(total), DataType.Amount)
    Right(newDataStack)
  }

  def balance(context: ExecutionContext)(address: DataEntry,
                                         tokenIdx: DataEntry,
                                         dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, address.data))
    val balance = context.state.tokenAccountBalance(id).getOrElse(0L)
    val newDataStack = dataStack :+ DataEntry(Longs.toByteArray(balance), DataType.Amount)
    Right(newDataStack)
  }

  object LoadType extends Enumeration {
    val IssuerLoad = Value(1)
    val SenderLoad = Value(2)
    val MaxLoad = Value(3)
    val TotalLoad = Value(4)
    val BalanceLoad = Value(5)
  }

}


