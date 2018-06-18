package scorex.transaction

import com.google.common.primitives.{Bytes, Longs, Ints}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}


case class ContendSlotsTransaction private(sender: PublicKeyAccount,
                                           slotid: Int,
                                           fee: Long,
                                           timestamp: Long,
                                           signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ContendSlotsTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    Ints.toByteArray(slotid),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp))

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "slotid" -> slotid,
    "fee" -> fee,
    "timestamp" -> timestamp
  )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object ContendSlotsTransaction {

  def parseTail(bytes: Array[Byte]): Try[ContendSlotsTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (slotBytes, slotidEnd) = (bytes.slice(KeyLength, KeyLength + SlotidLength), KeyLength + SlotidLength)
    val slotid = Ints.fromByteArray(slotBytes)
    val fee = Longs.fromByteArray(bytes.slice(slotidEnd, slotidEnd + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(slotidEnd + 8, slotidEnd + 16))
    val signature = ByteStr(bytes.slice(slotidEnd + 16, slotidEnd + 16 + SignatureLength))
    ContendSlotsTransaction
      .create(sender, slotid, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             slotid: Int,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ContendSlotsTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(ContendSlotsTransaction(sender, slotid, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             slotid: Int,
             fee: Long,
             timestamp: Long): Either[ValidationError, ContendSlotsTransaction] = {
    create(sender, slotid, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
