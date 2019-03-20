package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right, Try}

object TDBOpcDiff {

  def newToken(context: ExecutionContext)
              (stateVarMax: Array[Byte], stateVarTotal: Array[Byte], stateVarDesc: Array[Byte],
               max: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarMax, DataType.Amount) || !checkStateVar(stateVarTotal, DataType.Amount)
      || !checkStateVar(stateVarDesc, DataType.ShortText)) {
      Left(GenericError(s"wrong stateVariable"))
    } else if (max.dataType != DataType.Amount) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(max.data) < 0) {
      Left(GenericError("Invalid token max"))
    } else {
      val tokenIndex = context.state.contractTokens(context.contractId.bytes)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, Ints.toByteArray(tokenIndex)))
      Right(OpcDiff(
        tokenDB = Map(
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0)))) -> max.bytes,
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarDesc(0)))) -> context.description),// wrong description
        contractTokens = Map(context.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> 0L)
      ))
    }
  }

  def deposit(context: ExecutionContext)
             (stateVarTotal: Array[Byte], stateVarMax: Array[Byte], issuer: DataEntry,
              amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarTotal, DataType.Amount) || !checkStateVar(stateVarMax, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val depositAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0))))
      val currentTotal = context.state.tokenAccountBalance(tokenTotalKey)
      val tokenMaxKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0))))
      val tokenMax = Longs.fromByteArray(context.state.tokenInfo(tokenMaxKey).getOrElse(DataEntry(Longs.toByteArray(0), DataType.Amount)).data)
      if (Try(Math.addExact(depositAmount, currentTotal)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (depositAmount < 0) {
        Left(GenericError("Invalid deposit amount"))
      } else if (depositAmount + currentTotal > tokenMax) {
        Left(GenericError(s"New total ${depositAmount + currentTotal} is larger than the max $tokenMax"))
      } else {
        Right(OpcDiff(tokenAccountBalance = Map(tokenTotalKey -> depositAmount,
          ByteStr(Bytes.concat(tokenID.arr, issuer.data)) -> depositAmount)
        ))
      }
    }
  }

  def withdraw(context: ExecutionContext)
              (stateVarTotal: Array[Byte], issuer: DataEntry, amount: DataEntry,
               tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarTotal, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val withdrawAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val issuerBalanceKey = ByteStr(Bytes.concat(tokenID.arr, issuer.data))
      val issuerCurrentBalance = context.state.tokenAccountBalance(issuerBalanceKey)
      if (withdrawAmount > issuerCurrentBalance) {
        Left(GenericError(s"Amount $withdrawAmount is larger than the current balance $issuerCurrentBalance"))
      } else if (withdrawAmount < 0){
        Left(GenericError("Invalid withdraw amount"))
      }
      else {
        Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> -withdrawAmount,
          issuerBalanceKey -> -withdrawAmount)
        ))
      }
    }
  }

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry,
               amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if ((sender.dataType != DataType.Address) || (recipient.dataType != DataType.Address)
      || (amount.dataType !=  DataType.Amount) || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val transferAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val senderBalanceKey = ByteStr(Bytes.concat(tokenID.arr, sender.data))
      val senderCurrentBalance = context.state.tokenAccountBalance(senderBalanceKey)
      val recipientBalanceKey = ByteStr(Bytes.concat(tokenID.arr, recipient.data))
      val recipientCurrentBalance = context.state.tokenAccountBalance(recipientBalanceKey)
      if (transferAmount > senderCurrentBalance) {
        Left(GenericError(s"Amount $transferAmount is larger than the sender balance $senderCurrentBalance"))
      } else if (Try(Math.addExact(transferAmount, recipientCurrentBalance)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (transferAmount < 0) {
        Left(GenericError("Invalid transfer amount"))
      } else {
        Right(OpcDiff(tokenAccountBalance = Map(senderBalanceKey -> -transferAmount,
          recipientBalanceKey -> transferAmount)
        ))
      }
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
