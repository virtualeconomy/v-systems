package vsys.blockchain.transaction.lease

import com.google.common.primitives.{Bytes, Longs, Shorts}
import vsys.blockchain.state.ByteStr
import play.api.libs.json.{JsObject, Json}
import vsys.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.transaction.TransactionParser._
import vsys.blockchain.transaction._
import vsys.blockchain.transaction.ProvenTransaction
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.{Failure, Success, Try}

case class LeaseTransaction private(amount: Long,
                                    fee: Long,
                                    feeScale: Short,
                                    timestamp: Long,
                                    recipient: Address,
                                    proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.LeaseTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
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

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object LeaseTransaction {

  def parseTail(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    (for {
      recRes <- Address.fromBytes(bytes, 0)
      (recipient, recipientEnd) = recRes
      quantityStart = recipientEnd
      quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
      feeScale = Shorts.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 18))
      timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 18, quantityStart + 26))
      proofs <- Proofs.fromBytes(bytes.slice(quantityStart + 26, bytes.length))
      tx <- LeaseTransaction.createWithProof(quantity, fee, feeScale, timestamp, recipient, proofs)

    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(amount: Long,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             recipient: Address,
             proofs: Proofs): Either[ValidationError, LeaseTransaction] = {

    if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != DefaultFeeScale) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else if (recipient.isInstanceOf[Address]
      && !proofs.equals(Proofs.empty)
      && EllipticCurve25519Proof.fromBytes(proofs.proofs.head.bytes.arr).toOption.get.publicKey.stringRepr == recipient.stringRepr) {
      Left(ValidationError.ToSelf)
    } else {
      Right(LeaseTransaction(amount, fee, feeScale, timestamp, recipient, proofs))
    }
  }

  def create(sender: PrivateKeyAccount,
             amount: Long,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             recipient: Address): Either[ValidationError, LeaseTransaction] = for {
    unsigned <- createWithProof(amount, fee, feeScale, timestamp, recipient, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(amount, fee, feeScale, timestamp, recipient, proofs)
  } yield tx

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             feeScale: Short,
             timeStamp: Long,
             recipient: Address,
             signature: ByteStr): Either[ValidationError, LeaseTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(amount, fee, feeScale, timeStamp,recipient, proofs)
  } yield tx
}
