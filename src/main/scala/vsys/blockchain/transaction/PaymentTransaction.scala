package vsys.blockchain.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import vsys.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.transaction.TransactionParser._
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.blockchain.state.ByteStr
import vsys.utils.base58Length
import vsys.utils.serialization.{BytesSerializable, Deser}

import scala.util.{Failure, Success, Try}

case class PaymentTransaction private(recipient: Address,
                                      amount: Long,
                                      transactionFee: Long,
                                      feeScale: Short,
                                      timestamp: Long,
                                      attachment: Array[Byte],
                                      proofs: Proofs) extends ProvenTransaction with AmountInvolved {

  val transactionType = TransactionType.PaymentTransaction

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(transactionFee)
    val feeScaleBytes = Shorts.toByteArray(feeScale)

    Bytes.concat(Array(transactionType.id.toByte),
      timestampBytes,
      amountBytes,
      feeBytes,
      feeScaleBytes,
      recipient.bytes.arr,
      BytesSerializable.arrayWithSize(attachment)
    )
  }

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "recipient" -> recipient.stringRepr,
    "amount" -> amount,
    "attachment" -> Base58.encode(attachment)
  )

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object PaymentTransaction extends TransactionParser {

  val MaxAttachmentSize = 140
  val MaxAttachmentStringSize = base58Length(MaxAttachmentSize)
  val RecipientLength = Address.AddressLength

  def parseTail(bytes: Array[Byte]): Try[PaymentTransaction] = Try {

    val timestamp = Longs.fromByteArray(bytes.slice(0, 8))
    val amount = Longs.fromByteArray(bytes.slice(8, 16))
    val feeAmount = Longs.fromByteArray(bytes.slice(16, 24))
    val feeScale = Shorts.fromByteArray(bytes.slice(24, 26))
    val recipient = Address.fromBytes(bytes.slice(26, 26 + RecipientLength)).right.get
    val (attachment, toSignLength) = Deser.parseArraySize(bytes, 26 + RecipientLength)
    (for {
      proofs <- Proofs.fromBytes(bytes.slice(toSignLength, bytes.length))
      tx <- PaymentTransaction.createWithProof(recipient, amount, feeAmount, feeScale, timestamp, attachment, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(recipient: Address,
             amount: Long,
             feeAmount: Long,
             feeScale: Short,
             timestamp: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, PaymentTransaction] = {
    if (attachment.length > PaymentTransaction.MaxAttachmentSize) {
      Left(ValidationError.TooBigArray)
    } else if(amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (feeScale != DefaultFeeScale) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(PaymentTransaction(recipient, amount, feeAmount, feeScale, timestamp, attachment, proofs))
    }
  }

  def create(sender: PrivateKeyAccount,
             recipient: Address,
             amount: Long,
             feeAmount: Long,
             feeScale: Short,
             timestamp: Long,
             attachment: Array[Byte]): Either[ValidationError, PaymentTransaction] = for {
    unsigned <- createWithProof(recipient, amount, feeAmount, feeScale, timestamp, attachment, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(recipient, amount, feeAmount, feeScale, timestamp, attachment, proofs)
  } yield tx

    def create(sender: PublicKeyAccount,
               recipient: Address,
               amount: Long,
               feeAmount: Long,
               feeScale: Short,
               timestamp: Long,
               attachment: Array[Byte],
               signature: ByteStr): Either[ValidationError, PaymentTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(recipient, amount, feeAmount, feeScale, timestamp, attachment, proofs)
  } yield tx
}
