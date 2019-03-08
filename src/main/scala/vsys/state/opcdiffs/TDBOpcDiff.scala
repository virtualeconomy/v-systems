package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}
import scala.util.{Left, Right, Try}

object TDBOpcDiff {

  // max function
  //def max_token(tokenID: DataEntry)

  // issue tokens
  def deposit(tokenID: DataEntry,
              issuer: DataEntry,
              amount: DataEntry,
              total: DataEntry,
              max: DataEntry,
              issuerBalance: DataEntry): Either[ValidationError, OpcDiff] = {
    if ((tokenID.dataType != DataType.Address) || (issuer.dataType != DataType.Address)
      || (amount.dataType != DataType.Amount) || (total.dataType != DataType.Amount)
      || (max.dataType != DataType.Amount) || (issuerBalance.dataType != DataType.Amount)) {
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
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.data, Array("total".toByte))) -> Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.data, issuer.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }

  // destroy tokens
  def destroy(tokenID: DataEntry,
              issuer: DataEntry,
              amount: DataEntry,
              total: DataEntry,
              max: DataEntry,
              issuerBalance: DataEntry): Either[ValidationError, OpcDiff] = {
    if ((tokenID.dataType != DataType.Address) || (issuer.dataType != DataType.Address)
      || (amount.dataType != DataType.Amount) || (total.dataType != DataType.Amount)
      || (max.dataType !=  DataType.Amount) || (issuerBalance.dataType != DataType.Amount)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if ((Longs.fromByteArray(amount.data) <= 0) || (Longs.fromByteArray(amount.data) > Longs.fromByteArray(total.data))) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} should be larger than 0 and less than the total ${Longs.fromByteArray(total.data)}"))
    } else if (Longs.fromByteArray(amount.data) < Longs.fromByteArray(issuerBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the balance ${Longs.fromByteArray(issuerBalance.data)}"))
    } else {
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.data, Array("total".toByte))) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.data, issuer.data)) -> -Longs.fromByteArray(amount.data))
      ))
    }
  }

  // transfer token
  def transfer(tokenID: DataEntry,
               sender: DataEntry,
               senderBalance: DataEntry,
               recipient: DataEntry,
               recipientBalance: DataEntry,
               amount: DataEntry): Either[ValidationError, OpcDiff] = {
    if ((sender.dataType != DataType.Address) || (recipient.dataType != DataType.Address)
      || (senderBalance.dataType != DataType.Amount) || (recipientBalance.dataType != DataType.Amount)
      || (amount.dataType !=  DataType.Amount) || (tokenID.dataType != DataType.Address)) {
      Left(GenericError("Input contains invalid dataType"))
    } else if (Longs.fromByteArray(amount.data) <= 0) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} should be larger than 0"))
    } else if (Longs.fromByteArray(amount.data) > Longs.fromByteArray(senderBalance.data)) {
      Left(GenericError(s"Amount ${Longs.fromByteArray(amount.data)} is larger than the sender balance ${Longs.fromByteArray(senderBalance.data)}"))
    } else if (Try(Math.addExact(Longs.fromByteArray(amount.data), Longs.fromByteArray(recipientBalance.data))).isFailure) {
      Left(ValidationError.OverflowError)
    } else {
      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(tokenID.data, sender.data)) -> -Longs.fromByteArray(amount.data),
        ByteStr(Bytes.concat(tokenID.data, recipient.data)) -> Longs.fromByteArray(amount.data))
      ))
    }
  }
}