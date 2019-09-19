package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.account.Address
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.ExecutionContext

import scala.util.{Left, Right, Try}

object TDBAOpcDiff extends OpcDiffer {

  def deposit(context: ExecutionContext)
             (issuer: DataEntry, amount: DataEntry, tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {

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

  def withdraw(context: ExecutionContext)
              (issuer: DataEntry, amount: DataEntry, tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {

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

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry,
               tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {

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

  object TDBAType extends Enumeration {
    sealed case class TDBATypeVal(
      tdbaType: Int,
      operandCount: Int,
      differ: (ExecutionContext, Array[Byte], Seq[DataEntry]) => Either[ValidationError, OpcDiff])
    extends Val(tdbaType) { def *(n: Int): Int = n * tdbaType }

    val DepositTDBA  = TDBATypeVal(1, 3, (c, b, d) => deposit(c)(d(b(1)), d(b(2)), tokenIndex(b, d, 3)))
    val WithdrawTDBA = TDBATypeVal(2, 3, (c, b, d) => withdraw(c)(d(b(1)), d(b(2)), tokenIndex(b, d, 3)))
    val TransferTDBA = TDBATypeVal(3, 4, (c, b, d) => transfer(c)(d(b(1)), d(b(2)), d(b(3)), tokenIndex(b, d, 4)))

    def fromByte(implicit b: Byte): Option[TDBAType.TDBATypeVal] = Try(TDBAType(b).asInstanceOf[TDBATypeVal]).toOption

  }

  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    bytes.headOption.flatMap(TDBAType.fromByte(_)) match {
      case Some(t: TDBAType.TDBATypeVal) if checkData(bytes, data.length, t.operandCount) => t.differ(context, bytes, data)
      case _ => Left(ContractInvalidOPCData)
    }
}
