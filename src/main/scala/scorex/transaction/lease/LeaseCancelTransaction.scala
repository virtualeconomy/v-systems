package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction.TransactionParser.{KeyLength, _}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransaction private(sender: PublicKeyAccount,
                                          leaseId: ByteStr,
                                          fee: Long,
                                          feeScale: Short,
                                          timestamp: Long,
                                          signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.LeaseCancelTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
    sender.publicKey,
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp),
    leaseId.arr)

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp,
    "leaseId" -> leaseId.base58
  )

  // TODO
  // add feeScale in assetFee, need to change 100 later
  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object LeaseCancelTransaction {

  def parseTail(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val fee = Longs.fromByteArray(bytes.slice(KeyLength, KeyLength + 8))
    val feeScale = Shorts.fromByteArray(bytes.slice(KeyLength + 8, KeyLength + 10))
    val timestamp = Longs.fromByteArray(bytes.slice(KeyLength + 10, KeyLength + 18))
    val leaseId = ByteStr(bytes.slice(KeyLength + 18, KeyLength + 18 + DigestSize))
    val signature = ByteStr(bytes.slice(KeyLength + 18 + DigestSize, KeyLength + 18 + DigestSize + SignatureLength))
    LeaseCancelTransaction
      .create(sender, leaseId, fee, feeScale, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, LeaseCancelTransaction] =
    if (leaseId.arr.length != DigestSize) {
      Left(ValidationError.GenericError("Lease transaction id is invalid"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100){
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(LeaseCancelTransaction(sender, leaseId, fee, feeScale, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, LeaseCancelTransaction] = {
    create(sender, leaseId, fee, feeScale, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
