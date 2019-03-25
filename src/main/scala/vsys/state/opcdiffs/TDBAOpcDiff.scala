package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ExecutionContext
import vsys.contract.Contract.checkStateVar

import scala.util.{Left, Right, Try}

object TDBAOpcDiff {

  def deposit(context: ExecutionContext)
             (stateVarMax: Array[Byte], stateVarTotal: Array[Byte], issuer: DataEntry,
              amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (!checkStateVar(stateVarTotal, DataType.Amount) || !checkStateVar(stateVarMax, DataType.Amount)) {
      Left(GenericError(s"Wrong stateVariable"))
    } else if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (tokenIndex.dataType != DataType.Int32)) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val depositAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0))))
      val currentTotal = context.state.tokenAccountBalance(tokenTotalKey)
      val tokenMaxKey = ByteStr(Bytes.concat(tokenID.arr, Array(stateVarMax(0))))
      val tokenMax = Longs.fromByteArray(context.state.tokenInfo(tokenMaxKey).getOrElse(
        DataEntry(Longs.toByteArray(0), DataType.Amount)).data)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else if (Try(Math.addExact(depositAmount, currentTotal)).isFailure) {
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
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val withdrawAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val issuerBalanceKey = ByteStr(Bytes.concat(tokenID.arr, issuer.data))
      val issuerCurrentBalance = context.state.tokenAccountBalance(issuerBalanceKey)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else if (withdrawAmount > issuerCurrentBalance) {
        Left(GenericError(s"Amount $withdrawAmount is larger than the current balance $issuerCurrentBalance"))
      } else if (withdrawAmount < 0){
        Left(GenericError(s"Invalid withdraw amount $withdrawAmount"))
      }
      else {
        Right(OpcDiff(tokenAccountBalance = Map(issuerBalanceKey -> -withdrawAmount,
          ByteStr(Bytes.concat(tokenID.arr, Array(stateVarTotal(0)))) -> -withdrawAmount)
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
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val transferAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIndex.data))
      val senderBalanceKey = ByteStr(Bytes.concat(tokenID.arr, sender.data))
      val senderCurrentBalance = context.state.tokenAccountBalance(senderBalanceKey)
      val recipientBalanceKey = ByteStr(Bytes.concat(tokenID.arr, recipient.data))
      val recipientCurrentBalance = context.state.tokenAccountBalance(recipientBalanceKey)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(GenericError(s"Token $tokenNumber not exist"))
      } else if (transferAmount > senderCurrentBalance) {
        Left(GenericError(s"Amount $transferAmount is larger than the sender balance $senderCurrentBalance"))
      } else if (Try(Math.addExact(transferAmount, recipientCurrentBalance)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (transferAmount < 0) {
        Left(GenericError(s"Invalid transfer amount $transferAmount"))
      } else {
        Right(OpcDiff(tokenAccountBalance = Map(senderBalanceKey -> -transferAmount,
          recipientBalanceKey -> transferAmount)
        ))
      }
    }
  }

  object TDBAType extends Enumeration {
    val DepositTDBA = Value(1)
    val WithdrawTDBA = Value(2)
    val TransferTDBA = Value(3)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TDBAType.DepositTDBA.id && checkInput(bytes, 6, context.stateVar.length, data.length, 3) =>
      deposit(context)(context.stateVar(bytes(1)), context.stateVar(bytes(2)), data(bytes(3)), data(bytes(4)), data(bytes(5)))
    case opcType: Byte if opcType == TDBAType.WithdrawTDBA.id && checkInput(bytes, 5, context.stateVar.length, data.length, 2) =>
      withdraw(context)(context.stateVar(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)))
    case opcType: Byte if opcType == TDBAType.TransferTDBA.id && checkInput(bytes, 5, context.stateVar.length, data.length, 1) =>
      transfer(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)))
    case _ => Left(GenericError("Wrong TDBA opcode"))
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, stateVarLength: Int, dataLength: Int, sep: Int): Boolean = {
    bytes.length == bLength && bytes.slice(1, sep).forall(_ < stateVarLength) && bytes.slice(sep, bLength).forall(_ < dataLength) && bytes.tail.min >= 0
  }

}
