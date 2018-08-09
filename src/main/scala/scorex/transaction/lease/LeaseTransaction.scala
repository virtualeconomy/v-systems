package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressOrAlias, Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseTransaction private(sender: PublicKeyAccount,
                                    amount: Long,
                                    fee: Long,
                                    feeScale: Short,
                                    timestamp: Long,
                                    recipient: AddressOrAlias,
                                    signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.LeaseTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
    sender.publicKey,
    recipient.bytes.arr,
    Longs.toByteArray(amount),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "amount" -> amount,
    "recipient" -> recipient.stringRepr,
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp
  )

  // TODO
  // add feeScale in assetFee, need to change 100 later
  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, 100)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object LeaseTransaction {

  def parseTail(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    (for {
      recRes <- AddressOrAlias.fromBytes(bytes, KeyLength)
      (recipient, recipientEnd) = recRes
      quantityStart = recipientEnd
      quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
      feeScale = Shorts.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 18))
      timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
      signature = ByteStr(bytes.slice(quantityStart + 26, quantityStart + 26 + SignatureLength))
      lt <- LeaseTransaction.create(sender, quantity, fee, feeScale, timestamp, recipient, signature)
    } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             recipient: AddressOrAlias,
             signature: ByteStr): Either[ValidationError, LeaseTransaction] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100){
      Left(ValidationError.WrongFeeScale(feeScale))
    }  else if (recipient.isInstanceOf[Address] && sender.stringRepr == recipient.stringRepr) {
      Left(ValidationError.ToSelf)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(LeaseTransaction(sender, amount, fee, feeScale, timestamp, recipient, signature))
    }
  }

  def create(sender: PrivateKeyAccount,
             amount: Long,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             recipient: AddressOrAlias): Either[ValidationError, LeaseTransaction] = {
    create(sender, amount, fee, feeScale, timestamp, recipient, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
