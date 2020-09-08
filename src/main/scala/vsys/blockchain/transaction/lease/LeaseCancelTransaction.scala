package vsys.blockchain.transaction.lease

import com.google.common.primitives.{Bytes, Longs, Shorts}
import vsys.blockchain.state.ByteStr
import play.api.libs.json.{JsObject, Json}
import vsys.account.{PrivateKeyAccount, PublicKeyAccount}
import vsys.utils.crypto.hash.FastCryptographicHash.DigestSize
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.blockchain.transaction.TransactionParser._
import vsys.blockchain.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransaction private(leaseId: ByteStr,
                                          transactionFee: Long,
                                          feeScale: Short,
                                          timestamp: Long,
                                          proofs: Proofs) extends ProvenTransaction {

  val transactionType = TransactionType.LeaseCancelTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
    Longs.toByteArray(transactionFee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp),
    leaseId.arr)

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "timestamp" -> timestamp,
    "leaseId" -> leaseId.base58
  )

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object LeaseCancelTransaction extends TransactionParser {

  def parseTail(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, 8))
    val feeScale = Shorts.fromByteArray(bytes.slice(8, 10))
    val timestamp = Longs.fromByteArray(bytes.slice(10, 18))
    val leaseId = ByteStr(bytes.slice(18, 18 + DigestSize))
    (for {
      proofs <- Proofs.fromBytes(bytes.slice(18 + DigestSize, bytes.length))
      tx <- LeaseCancelTransaction.createWithProof(leaseId, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(leaseId: ByteStr,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, LeaseCancelTransaction] =
    if (leaseId.arr.length != DigestSize) {
      Left(ValidationError.GenericError("Lease transaction id is invalid"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != DefaultFeeScale){
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(LeaseCancelTransaction(leaseId, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, LeaseCancelTransaction] = for {
    unsigned <- createWithProof(leaseId, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(leaseId, fee, feeScale, timestamp, proofs)
  } yield tx

  def create(sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, LeaseCancelTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(leaseId, fee, feeScale, timestamp, proofs)
  } yield tx
}
