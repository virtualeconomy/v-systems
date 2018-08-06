package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class PaymentTransaction private(sender: PublicKeyAccount,
                                      recipient: Address,
                                      amount: Long,
                                      fee: Long,
                                      timestamp: Long,
                                      signature: ByteStr) extends SignedTransaction {
  override val transactionType = TransactionType.PaymentTransaction
  // TODO
  // add feeScale in assetFee, need to change 100 later
  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, 100)

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)

    Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      timestampBytes,
      amountBytes,
      feeBytes,
      recipient.bytes.arr)
  }

  override lazy val json: JsObject =jsonBase() ++ Json.obj(
    "recipient" -> recipient.stringRepr,
    "amount" -> amount,
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
    val recipient = Address.fromBytes(bytes.slice(s1 + 24, s1 + 24 + RecipientLength)).right.get
    PaymentTransaction
      .create(sender, recipient, amount, feeAmount, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             recipient: Address,
             amount: Long,
             feeAmount: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, PaymentTransaction] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else {
      Right(PaymentTransaction(sender, recipient, amount, feeAmount, timestamp, signature))
    }
  }

  def create(sender: PrivateKeyAccount,
             recipient: Address,
             amount: Long,
             feeAmount: Long,
             timestamp: Long): Either[ValidationError, PaymentTransaction] = {
    create( sender, recipient, amount, feeAmount, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
