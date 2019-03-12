package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import vsys.contract.ContractContext
import scala.util.{Left, Right, Try}

object TDBOpcDiff {
  def newToken(contractContext: ContractContext)
              (issuer: DataEntry,
               total: DataEntry,
               max: DataEntry): Either[ValidationError, OpcDiff] = {
    if ((issuer.dataType != DataType.Address) || (total.dataType != DataType.Amount)
      || (max.dataType != DataType.Amount)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(total.data) <= 0)  {
      Left(GenericError(s"Total ${Longs.fromByteArray(total.data)} should be larger than 0"))
    } else if (Longs.fromByteArray(max.data) <= 0)  {
      Left(GenericError(s"Max ${Longs.fromByteArray(max.data)} should be larger than 0"))
    } else if (Longs.fromByteArray(total.data) > Longs.fromByteArray(max.data)) {
      Left(GenericError(s"Max ${Longs.fromByteArray(max.data)} should be no smaller than the total ${Longs.fromByteArray(total.data)}"))
    } else {
      val tokenID: ByteStr = ByteStr(Bytes.concat(contractContext.contractId.bytes.arr, Ints.toByteArray(contractContext.state.contractTokens(contractContext.contractId.bytes).get + 1)))
      Right(OpcDiff(tokenDB = Map(ByteStr(Bytes.concat(tokenID.arr, Array("max".toByte))) -> max.data,
        ByteStr(Bytes.concat(tokenID.arr, Array("issuer".toByte))) -> issuer.data,
        ByteStr(Bytes.concat(tokenID.arr, Array("desc".toByte))) -> contractContext.description),
        contractTokens = Map(contractContext.contractId.bytes -> 1),
        tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array("total".toByte))) -> 0L)
      ))
    }
  }

  def deposit(contractContext: ContractContext)
             (issuer: DataEntry,
              amount: DataEntry,
              total: DataEntry,
              max: DataEntry,
              issuerBalance: DataEntry,
              tokenIndex: DataEntry): Either[ValidationError, OpcDiff] = {
    val tokenID: ByteStr = ByteStr(Bytes.concat(contractContext.contractId.bytes.arr, Ints.toByteArray(contractContext.state.contractTokens(contractContext.contractId.bytes).get + 1)))

    if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount)
      || (total.dataType != DataType.Amount) || (max.dataType != DataType.Amount) || (issuerBalance.dataType != DataType.Amount)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(amount.data) <= 0) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} should be larger than 0"))
    } else if (Try(Math.addExact(Longs.fromByteArray(amount.data), Longs.fromByteArray(total.data))).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (Longs.fromByteArray(amount.data) + Longs.fromByteArray(total.data) > Longs.fromByteArray(max.data)) {
      Left(GenericError(s"New total ${Longs.fromByteArray(amount.data) + Longs.fromByteArray(total.data)} is larger than the max ${Longs.fromByteArray(max.data)}"))
    } else if (Longs.fromByteArray(amount.data) + Longs.fromByteArray(issuerBalance.data) > Longs.fromByteArray(max.data)) {
      Left(GenericError(s"New total ${Longs.fromByteArray(amount.data) + Longs.fromByteArray(issuerBalance.data)} is larger than the max ${Longs.fromByteArray(max.data)}"))
    } else {
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array("total".toByte))) -> Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, issuer.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }

  def destroy(contractContext: ContractContext)
             (tokenID: DataEntry,
              issuer: DataEntry,
              amount: DataEntry,
              total: DataEntry,
              max: DataEntry,
              issuerBalance: DataEntry): Either[ValidationError, OpcDiff] = {
    val tokenID: ByteStr = ByteStr(Bytes.concat(contractContext.contractId.bytes.arr, Ints.toByteArray(contractContext.state.contractTokens(contractContext.contractId.bytes).get + 1)))

    if ((issuer.dataType != DataType.Address) || (amount.dataType != DataType.Amount) || (total.dataType != DataType.Amount)
      || (max.dataType !=  DataType.Amount) || (issuerBalance.dataType != DataType.Amount)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if ((Longs.fromByteArray(amount.data) <= 0) || (Longs.fromByteArray(amount.data) > Longs.fromByteArray(total.data))) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} should be larger than 0 and less than the total ${Longs.fromByteArray(total.data)}"))
    } else if (Longs.fromByteArray(amount.data) < Longs.fromByteArray(issuerBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the balance ${Longs.fromByteArray(issuerBalance.data)}"))
    } else {
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, Array("total".toByte))) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, issuer.data)) -> -Longs.fromByteArray(amount.data))
      ))
    }
  }

  def transfer(contractContext: ContractContext)
              (tokenID: DataEntry,
               sender: DataEntry,
               senderBalance: DataEntry,
               recipient: DataEntry,
               recipientBalance: DataEntry,
               amount: DataEntry): Either[ValidationError, OpcDiff] = {
    val tokenID: ByteStr = ByteStr(Bytes.concat(contractContext.contractId.bytes.arr, Ints.toByteArray(contractContext.state.contractTokens(contractContext.contractId.bytes).get + 1)))

    if ((sender.dataType != DataType.Address) || (recipient.dataType != DataType.Address)
      || (senderBalance.dataType != DataType.Amount) || (recipientBalance.dataType != DataType.Amount)
      || (amount.dataType !=  DataType.Amount)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(amount.data) <= 0) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} should be larger than 0"))
    } else if (Longs.fromByteArray(amount.data) > Longs.fromByteArray(senderBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the sender balance ${Longs.fromByteArray(senderBalance.data)}"))
    } else if (Try(Math.addExact(Longs.fromByteArray(amount.data), Longs.fromByteArray(recipientBalance.data))).isFailure) {
      Left(ValidationError.OverflowError)
    } else {
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.arr, sender.data)) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.arr, recipient.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }
}