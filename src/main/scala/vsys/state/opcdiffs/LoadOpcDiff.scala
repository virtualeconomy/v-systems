package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.{GenericError, InvalidDataEntry}
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.Right

object LoadOpcDiff {

  def issuer(context: ExecutionContext)(stateVarIssuer: Array[Byte],
                                        dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    if (stateVarIssuer.length != 2 || DataType.fromByte(stateVarIssuer(1)).get != DataType.Address) {
      Left(GenericError(s"Wrong stateVariable $stateVarIssuer"))
    } else {
      context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVarIssuer(0)))) match {
        case Some(i) => Right(dataStack :+ i)
        case _ => Left(GenericError(s"${context.contractId.address}'s issuer not defined"))
      }
    }
  }

  def sender(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    Right(dataStack :+ DataEntry(context.signers.head.bytes.arr, DataType.Address))
  }

  def max(context: ExecutionContext)(stateVarMax: Array[Byte],
                                     tokenIdx: DataEntry,
                                     dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    if (stateVarMax.length != 2 || DataType.fromByte(stateVarMax(1)).get != DataType.Amount) {
      Left(GenericError(s"Wrong stateVariable $stateVarMax"))
    } else {
      val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array(stateVarMax(0))))
      context.state.tokenInfo(id) match {
        case Some(m) if m.dataType == DataType.fromByte(stateVarMax(1)).get => Right(dataStack :+ m)
        case _ => Left(InvalidDataEntry)
      }
    }
  }

  def total(context: ExecutionContext)(stateVarTotal: Array[Byte],
                                       tokenIdx: DataEntry,
                                       dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    if (stateVarTotal.length != 2) {
      Left(GenericError(s"Wrong stateVariable $stateVarTotal"))
    } else {
      val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array(stateVarTotal(0))))
      val total = context.state.tokenAccountBalance(id)
      Right(dataStack :+ DataEntry(Longs.toByteArray(total), DataType.Amount))
    }
  }

  def balance(context: ExecutionContext)(address: DataEntry,
                                         tokenIdx: DataEntry,
                                         dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, address.data))
    val balance = context.state.tokenAccountBalance(id)
    Right(dataStack :+ DataEntry(Longs.toByteArray(balance), DataType.Amount))
  }

  object LoadType extends Enumeration {
    val IssuerLoad = Value(1)
    val SenderLoad = Value(2)
    val MaxLoad = Value(3)
    val TotalLoad = Value(4)
    val BalanceLoad = Value(5)
  }

}
