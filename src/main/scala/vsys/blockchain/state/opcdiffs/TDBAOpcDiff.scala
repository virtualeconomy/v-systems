package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.account.{Address, ContractAccount}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{CallType, DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object TDBAOpcDiff {

  def deposit(context: ExecutionContext)
             (issuer: DataEntry, amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (tokenIndex.dataType != DataType.Int32)) {
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val depositAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(1.toByte)))
      val issuerBalanceKey = ByteStr(Bytes.concat(tokenID.arr, issuer.data))
      val currentTotal = context.state.tokenAccountBalance(tokenTotalKey)
      val tokenMaxKey = ByteStr(Bytes.concat(tokenID.arr, Array(0.toByte)))
      val tokenMax = Longs.fromByteArray(context.state.tokenInfo(tokenMaxKey).getOrElse(
        DataEntry(Longs.toByteArray(0), DataType.Amount)).data)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else if (Try(Math.addExact(depositAmount, currentTotal)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (depositAmount < 0) {
        Left(ContractInvalidAmount)
      } else if (depositAmount + currentTotal > tokenMax) {
        Left(ContractTokenMaxExceeded)
      } else {
        val a = Address.fromBytes(issuer.data).toOption.get
        Right(OpcDiff(relatedAddress = Map(a -> true),
          tokenAccountBalance = Map(tokenTotalKey -> depositAmount, issuerBalanceKey -> depositAmount)))
      }
    }
  }

  def depositWithoutTokenIndex(context: ExecutionContext)
                              (issuer: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    deposit(context)(issuer, amount, tokenIndex)
  }

  def withdraw(context: ExecutionContext)
              (issuer: DataEntry, amount: DataEntry, tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (tokenIndex.dataType != DataType.Int32)) {
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val withdrawAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenTotalKey = ByteStr(Bytes.concat(tokenID.arr, Array(1.toByte)))
      val issuerBalanceKey = ByteStr(Bytes.concat(tokenID.arr, issuer.data))
      val issuerCurrentBalance = context.state.tokenAccountBalance(issuerBalanceKey)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else if (withdrawAmount > issuerCurrentBalance) {
        Left(ContractTokenBalanceInsufficient)
      } else if (withdrawAmount < 0){
        Left(ContractInvalidAmount)
      }
      else {
        val a = Address.fromBytes(issuer.data).toOption.get
        Right(OpcDiff(relatedAddress = Map(a -> true),
          tokenAccountBalance = Map(tokenTotalKey -> -withdrawAmount, issuerBalanceKey -> -withdrawAmount)
        ))
      }
    }
  }

  def withdrawWithoutTokenIndex(context: ExecutionContext)
                               (issuer: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    withdraw(context)(issuer, amount, tokenIndex)
  }

  def getTriggerCallOpcDiff(context: ExecutionContext, diff: OpcDiff,
                            sender: DataEntry, recipient: DataEntry, amount: DataEntry,
                            callType: CallType.Value, callIndex: Int): Either[ValidationError, OpcDiff] = {
    if (callType == CallType.Trigger){
      if (callIndex == 1) {
        if (recipient.dataType == DataType.Address) Right(OpcDiff.empty)
        else {
          val senderContractId = ContractAccount.fromBytes(sender.data).toOption.get
          CallOpcDiff(context, diff, senderContractId, Seq(recipient, amount), callType, callIndex)
        }
      } else if (callIndex == 2){
        if (sender.dataType == DataType.Address) Right(OpcDiff.empty)
        else {
          val recipientContractId = ContractAccount.fromBytes(recipient.data).toOption.get
          CallOpcDiff(context, diff, recipientContractId, Seq(sender, amount), callType, callIndex)
        }
      } else {
        Left(GenericError("Invalid Call Index"))
      }
    } else {
      Left(GenericError("Invalid Call Type"))
    }
  }

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry,
               tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {

    if (sender.dataType == DataType.ContractAccount) {
      Left(ContractUnsupportedWithdraw)
    } else if (recipient.dataType == DataType.ContractAccount) {
      Left(ContractUnsupportedDeposit)
    } else if ((sender.dataType != DataType.Address) || (recipient.dataType != DataType.Address) ||
      (amount.dataType !=  DataType.Amount) || (tokenIndex.dataType != DataType.Int32)) {
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val transferAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val senderBalanceKey = ByteStr(Bytes.concat(tokenID.arr, sender.data))
      val senderCurrentBalance = context.state.tokenAccountBalance(senderBalanceKey)
      val recipientBalanceKey = ByteStr(Bytes.concat(tokenID.arr, recipient.data))
      val recipientCurrentBalance = context.state.tokenAccountBalance(recipientBalanceKey)
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else if (transferAmount > senderCurrentBalance) {
        Left(ContractTokenBalanceInsufficient)
      } else if (Try(Math.addExact(transferAmount, recipientCurrentBalance)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (transferAmount < 0) {
        Left(ContractInvalidAmount)
      } else {
        val s = Address.fromBytes(sender.data).toOption.get
        val r = Address.fromBytes(recipient.data).toOption.get
        if (sender.bytes sameElements recipient.bytes) {
          Right(OpcDiff(relatedAddress = Map(s -> true, r -> true)
          ))
        } else {
          Right(OpcDiff(relatedAddress = Map(s -> true, r -> true),
            tokenAccountBalance = Map(senderBalanceKey -> -transferAmount,
              recipientBalanceKey -> transferAmount)
          ))
        }
      }
    }
  }

  def transferWithoutTokenIndex(context: ExecutionContext)
                               (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    transfer(context)(sender, recipient, amount, tokenIndex)
  }

  object TDBAType extends Enumeration {
    val DepositTDBA = Value(1)
    val WithdrawTDBA = Value(2)
    val TransferTDBA = Value(3)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == TDBAType.DepositTDBA.id && checkInput(bytes,3, data.length) =>
      depositWithoutTokenIndex(context)(data(bytes(1)), data(bytes(2)))
    case opcType: Byte if opcType == TDBAType.DepositTDBA.id && checkInput(bytes,4, data.length) =>
      deposit(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case opcType: Byte if opcType == TDBAType.WithdrawTDBA.id && checkInput(bytes,3, data.length) =>
      withdrawWithoutTokenIndex(context)(data(bytes(1)), data(bytes(2)))
    case opcType: Byte if opcType == TDBAType.WithdrawTDBA.id && checkInput(bytes,4, data.length) =>
      withdraw(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case opcType: Byte if opcType == TDBAType.TransferTDBA.id && checkInput(bytes,4, data.length) =>
      transferWithoutTokenIndex(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)))
    case opcType: Byte if opcType == TDBAType.TransferTDBA.id && checkInput(bytes,5, data.length) =>
      transfer(context)(data(bytes(1)), data(bytes(2)), data(bytes(3)), data(bytes(4)))
    case _ => Left(ContractInvalidOPCData)
  }

  private def checkInput(bytes: Array[Byte], bLength: Int, dataLength: Int): Boolean = {
    bytes.length == bLength && bytes.tail.max < dataLength && bytes.tail.min >= 0
  }

}
