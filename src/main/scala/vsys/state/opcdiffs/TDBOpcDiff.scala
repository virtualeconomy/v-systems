package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext

import scala.util.{Left, Right, Try}

object TDBOpcDiff {

  def newToken(context: ExecutionContext)
              (stateVarMax: Array[Byte], stateVarTotal: Array[Byte], stateVarDesc: Array[Byte],
               max: DataEntry): Either[ValidationError, OpcDiff] = {

    if (stateVarMax.length != 2 || stateVarTotal.length != 2 || stateVarDesc.length != 2
      || DataType.fromByte(stateVarTotal(1)).get != DataType.Amount || DataType.fromByte(stateVarMax(1)).get != DataType.Amount
      || DataType.fromByte(stateVarDesc(1)).get != DataType.ShortText) {
      Left(GenericError(s"wrong stateVariable $stateVarTotal"))
    } else if (max.dataType != DataType.Amount) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, Ints.toByteArray(context.state.contractTokens(context.contractId.bytes))))
      Right(OpcDiff(tokenDB = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0)))) -> max.bytes,
        ByteStr(Bytes.concat(tokenID.arr, Array(stateVarDesc(0)))) -> context.description),// wrong description
        contractTokens = Map(context.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> 0L)
      ))
    }
  }

  def deposit(context: ExecutionContext)
             (stateVarTotal: Array[Byte], issuer: DataEntry, amount: DataEntry, total: DataEntry,
              max: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (stateVarTotal.length != 2 || DataType.fromByte(stateVarTotal(1)).get != DataType.Amount) {
      Left(GenericError(s"Wrong stateVariable $stateVarTotal"))
    } else if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (max.dataType != DataType.Amount) || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Try(Math.addExact(Longs.fromByteArray(amount.data), Longs.fromByteArray(total.data))).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (Longs.fromByteArray(amount.data) + Longs.fromByteArray(total.data) > Longs.fromByteArray(max.data)) {
      Left(GenericError(s"New total ${Longs.fromByteArray(amount.data) + Longs.fromByteArray(total.data)} is larger than the max ${Longs.fromByteArray(max.data)}"))
    } else {
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, issuer.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }

  def withdraw(context: ExecutionContext)
              (stateVarTotal: Array[Byte], issuer: DataEntry, amount: DataEntry,
               issuerBalance: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (stateVarTotal.length != 2 || DataType.fromByte(stateVarTotal(1)).get != DataType.Amount) {
      Left(GenericError(s"Wrong stateVariable $stateVarTotal"))
    } else if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (issuerBalance.dataType != DataType.Amount) || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(amount.data) > Longs.fromByteArray(issuerBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the balance ${Longs.fromByteArray(issuerBalance.data)}"))
    } else {
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, issuer.data)) -> -Longs.fromByteArray(amount.data))
      ))
    }
  }

  def transfer(context: ExecutionContext)
              (sender: DataEntry, senderBalance: DataEntry, recipient: DataEntry, recipientBalance: DataEntry,
               amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if ((sender.dataType != DataType.Address) || (recipient.dataType != DataType.Address)
      || (senderBalance.dataType != DataType.Amount) || (recipientBalance.dataType != DataType.Amount)
      || (amount.dataType !=  DataType.Amount) || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(amount.data) > Longs.fromByteArray(senderBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the sender balance ${Longs.fromByteArray(senderBalance.data)}"))
    } else if (Try(Math.addExact(Longs.fromByteArray(amount.data), Longs.fromByteArray(recipientBalance.data))).isFailure) {
      Left(ValidationError.OverflowError)
    } else {
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, sender.data)) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, recipient.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }

  object TDBType extends Enumeration {
    val NewTokenTDB = Value(1)
    val DepositTDB = Value(2)
    val DestroyTDB = Value(3)
    val TransferTDB = Value(4)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TDBType.NewTokenTDB.id && bytes.length == 5
      && bytes.slice(1, 4).max < context.stateVar.length && bytes.last < data.length && bytes.tail.min >= 0 =>
      newToken(context)(context.stateVar(bytes(1)), context.stateVar(bytes(2)), context.stateVar(bytes(3)), data(bytes(4)))
    case opcType: Byte if opcType == TDBType.DepositTDB.id && bytes.length == 7
      && bytes(1) < context.stateVar.length && bytes.slice(2, 7).max < data.length && bytes.tail.min >= 0 =>
      deposit(context)(context.stateVar(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)), data(bytes(5)), data(bytes(6)))
    case opcType: Byte if opcType == TDBType.DestroyTDB.id && bytes.length == 6
      && bytes(1) < context.stateVar.length && bytes.slice(2, 6).max < data.length && bytes.tail.min >= 0 =>
      withdraw(context)(context.stateVar(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)), data(bytes(5)))
    case opcType: Byte if opcType == TDBType.TransferTDB.id && bytes.length == 7
      && bytes.tail.max < context.stateVar.length && bytes.tail.min >= 0 =>
      transfer(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)), data(bytes(5)), data(bytes(6)))
    case _ => Left(GenericError("Wrong opcode"))
  }

}
