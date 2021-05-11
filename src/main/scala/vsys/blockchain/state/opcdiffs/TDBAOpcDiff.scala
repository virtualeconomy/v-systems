package vsys.blockchain.state.opcdiffs

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.account.{Account, Address, ContractAccount}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{CallType, DataEntry, DataType, ExecutionContext}

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
        val a = Address.fromBytes(issuer.data).explicitGet()
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
        val a = Address.fromBytes(issuer.data).explicitGet()
        Right(OpcDiff(relatedAddress = Map(a -> true),
          tokenAccountBalance = Map(tokenTotalKey -> -withdrawAmount, issuerBalanceKey -> -withdrawAmount)
        ))
      }
    }
  }

  def getTriggerCallOpcDiff(context: ExecutionContext, diff: OpcDiff,
                            sender: DataEntry, recipient: DataEntry, amount: DataEntry, tokenId: DataEntry,
                            callType: CallType.Value, callIndex: Int): Either[ValidationError, OpcDiff] = {
    if (callType == CallType.Trigger){
      callIndex match {
        case 2 =>
          if (sender.dataType == DataType.Address) Right(OpcDiff.empty)
          else {
            val senderContractId = ContractAccount.fromBytes(sender.data).explicitGet()
            CallOpcDiff(context, diff, senderContractId, Seq(recipient, amount, tokenId), callType, 2)
          }
        case 1 =>
          if (recipient.dataType == DataType.Address) Right(OpcDiff.empty)
          else {
            val recipientContractId = ContractAccount.fromBytes(recipient.data).explicitGet()
            CallOpcDiff(context, diff, recipientContractId, Seq(sender, amount, tokenId), callType, 1)
          }
        case _ => Left(GenericError("Invalid Call Index"))
      }
    } else {
      Left(GenericError("Invalid Call Type"))
    }
  }

  def contractTransfer(context: ExecutionContext)
                      (sender: DataEntry, recipient: DataEntry, amount: DataEntry,
                       tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {
    val dType = Array(amount.dataType.id.toByte, tokenIndex.dataType.id.toByte, sender.dataType.id.toByte, recipient.dataType.id.toByte)
    val rType = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte, DataType.Account.id.toByte, DataType.Account.id.toByte)

    if (!DataType.checkTypes(dType, rType)) {
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenIndexNumber = Ints.fromByteArray(tokenIndex.data)
      val transferAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).explicitGet()
      val tokenIdDataEntry = DataEntry(tokenID.arr, DataType.TokenId)
      val senderBalanceKey = ByteStr(Bytes.concat(tokenID.arr, sender.data))
      val senderCurrentBalance = context.state.tokenAccountBalance(senderBalanceKey)
      val recipientBalanceKey = ByteStr(Bytes.concat(tokenID.arr, recipient.data))
      val recipientCurrentBalance = context.state.tokenAccountBalance(recipientBalanceKey)
      if (tokenIndexNumber >= contractTokens || tokenIndexNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else if (transferAmount > senderCurrentBalance) {
        Left(ContractTokenBalanceInsufficient)
      } else if (Try(Math.addExact(transferAmount, recipientCurrentBalance)).isFailure) {
        Left(ValidationError.OverflowError)
      } else if (transferAmount < 0) {
        Left(ContractInvalidAmount)
      } else {
        // TODO
        // relatedContract needed
        for {
          senderCallDiff <- getTriggerCallOpcDiff(context, OpcDiff.empty, sender, recipient, amount, tokenIdDataEntry, CallType.Trigger, 2)
          senderRelatedAddress = Map[Account, Boolean](Account.fromBytes(sender.data,0).explicitGet()._1 -> true)
          senderDiff = OpcDiff(relatedAddress = senderRelatedAddress,
            tokenAccountBalance = Map(senderBalanceKey -> -transferAmount))
          senderTotalDiff = OpcDiff.opcDiffMonoid.combine(senderCallDiff, senderDiff)
          recipientCallDiff <- getTriggerCallOpcDiff(context, senderTotalDiff, sender, recipient, amount, tokenIdDataEntry, CallType.Trigger, 1)
          recipientRelatedAddress = Map[Account, Boolean](Account.fromBytes(recipient.data,0).explicitGet()._1 -> true)
          recipientDiff = OpcDiff(relatedAddress = recipientRelatedAddress,
            tokenAccountBalance = Map(recipientBalanceKey -> transferAmount))
          returnDiff = Monoid.combineAll(Seq(senderTotalDiff, recipientCallDiff, recipientDiff))
        } yield returnDiff
      }
    }
  }

  def basicTransfer(context: ExecutionContext)
                   (sender: DataEntry, recipient: DataEntry, amount: DataEntry,
                    tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {

    val dType = Array(amount.dataType.id.toByte, tokenIndex.dataType.id.toByte, sender.dataType.id.toByte, recipient.dataType.id.toByte)
    val rType = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte, DataType.Address.id.toByte, DataType.Address.id.toByte)
    if (sender.dataType == DataType.ContractAccount) {
      Left(ContractUnsupportedWithdraw)
    } else if (recipient.dataType == DataType.ContractAccount) {
      Left(ContractUnsupportedDeposit)
    } else if (!DataType.checkTypes(dType, rType)) {
      Left(ContractDataTypeMismatch)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val transferAmount = Longs.fromByteArray(amount.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).explicitGet()
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
        val s = Address.fromBytes(sender.data).explicitGet()
        val r = Address.fromBytes(recipient.data).explicitGet()
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

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry,
               tokenIndex: DataEntry = defaultTokenIndex): Either[ValidationError, OpcDiff] = {
    if (context.height <= context.settings.allowDepositWithdrawContractAfterHeight)
      basicTransfer(context)(sender, recipient, amount, tokenIndex)
    else contractTransfer(context)(sender, recipient, amount, tokenIndex)
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
