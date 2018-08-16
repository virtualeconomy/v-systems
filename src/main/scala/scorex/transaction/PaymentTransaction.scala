package scorex.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class PaymentTransaction private(sender: PublicKeyAccount,
                                      recipient: Address,
                                      amount: Long,
                                      fee: Long,
                                      feeScale: Short,
                                      timestamp: Long,
                                      attachment: Array[Byte],
                                      signature: ByteStr) extends SignedTransaction {
  override val transactionType = TransactionType.PaymentTransaction

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)
    val feeScaleBytes = Shorts.toByteArray(feeScale)

    Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      timestampBytes,
      amountBytes,
      feeBytes,
      feeScaleBytes,
      recipient.bytes.arr,
      BytesSerializable.arrayWithSize(attachment)
    )
  }

  override lazy val json: JsObject =jsonBase() ++ Json.obj(
    "recipient" -> recipient.stringRepr,
    "feeScale" -> feeScale,
    "amount" -> amount,
    "attachment" -> Base58.encode(attachment)
  )

  override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature.arr, toSign)

}

object PaymentTransaction {

  val MaxAttachmentSize = 140
  val MaxAttachmentStringSize = base58Length(MaxAttachmentSize)
  val RecipientLength = Address.AddressLength

  def parseTail(bytes: Array[Byte]): Try[PaymentTransaction] = Try {
    import EllipticCurveImpl._

    val signature = ByteStr(bytes.slice(0, SignatureLength))
    val txId = bytes(SignatureLength)
    require(txId == TransactionType.PaymentTransaction.id.toByte, s"Signed tx id is not match")
    val sender = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val s1 = SignatureLength + KeyLength + 1
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val feeAmount = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    val feeScale = Shorts.fromByteArray(bytes.slice(s1 + 24, s1 + 26))
    val recipient = Address.fromBytes(bytes.slice(s1 + 26, s1 + 26 + RecipientLength)).right.get
    val (attachment, _) = Deser.parseArraySize(bytes, s1 + 26 + RecipientLength)
    PaymentTransaction
      .create(sender, recipient, amount, feeAmount, feeScale, timestamp, attachment, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             recipient: Address,
             amount: Long,
             feeAmount: Long,
             feeScale: Short,
             timestamp: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, PaymentTransaction] = {
    if (attachment.length > PaymentTransaction.MaxAttachmentSize) {
      Left(ValidationError.TooBigArray)
    } else if(amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(PaymentTransaction(sender, recipient, amount, feeAmount, feeScale, timestamp, attachment, signature))
    }
  }

  def create(sender: PrivateKeyAccount,
             recipient: Address,
             amount: Long,
             feeAmount: Long,
             feeScale: Short,
             timestamp: Long,
             attachment: Array[Byte]): Either[ValidationError, PaymentTransaction] = {
    create( sender, recipient, amount, feeAmount, feeScale, timestamp, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
