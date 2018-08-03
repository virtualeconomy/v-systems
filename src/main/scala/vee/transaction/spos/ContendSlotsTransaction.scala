package vee.transaction.spos

import com.google.common.primitives.{Bytes, Longs, Ints, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.TransactionParser._
import scorex.transaction._

import scala.util.{Failure, Success, Try}


case class ContendSlotsTransaction private(sender: PublicKeyAccount,
                                           slotId: Int,
                                           fee: Long,
                                           feeScale: Short,
                                           timestamp: Long,
                                           signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ContendSlotsTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    Ints.toByteArray(slotId),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp))

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    //need to write more data here?
    "slotId" -> slotId,
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp
  )

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object ContendSlotsTransaction {

  def parseTail(bytes: Array[Byte]): Try[ContendSlotsTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (slotBytes, slotIdEnd) = (bytes.slice(KeyLength, KeyLength + SlotIdLength), KeyLength + SlotIdLength)
    val slotId = Ints.fromByteArray(slotBytes)
    val fee = Longs.fromByteArray(bytes.slice(slotIdEnd, slotIdEnd + 8))
    val feeScale = Shorts.fromByteArray(bytes.slice(slotIdEnd + 8, slotIdEnd + 10))
    val timestamp = Longs.fromByteArray(bytes.slice(slotIdEnd + 10, slotIdEnd + 18))
    val signature = ByteStr(bytes.slice(slotIdEnd + 18, slotIdEnd + 18 + SignatureLength))
    ContendSlotsTransaction
      .create(sender, slotId, fee, feeScale, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             slotId: Int,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ContendSlotsTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100){
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(ContendSlotsTransaction(sender, slotId, fee, feeScale, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             slotId: Int,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, ContendSlotsTransaction] = {
    create(sender, slotId, fee, feeScale, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
