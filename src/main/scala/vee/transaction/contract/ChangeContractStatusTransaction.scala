package vee.transaction.contract

import java.lang.String

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.{AssetId, ValidationError}
import scorex.transaction.TransactionParser.TransactionType
import vee.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vee.transaction.ProvenTransaction

import scala.util.{Failure, Success, Try}

case class ChangeContractStatusTransaction private(contractName: String,
                                              action: ChangeContractStatusAction.Value,
                                              fee: Long,
                                              feeScale: Short,
                                              timestamp: Long,
                                              proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ChangeContractStatusTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    BytesSerializable.arrayWithSize(contractName.getBytes("UTF-8")),
    Array(action.id.toByte),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "contract" -> contractName,
    "fee" -> fee,
    "feeScale" -> feeScale,
    "action" -> action.toString(),
    "timestamp" -> timestamp
  )

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, 100)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)
}

object ChangeContractStatusTransaction {

  def parseTail(bytes: Array[Byte]): Try[ChangeContractStatusTransaction] = Try {
    val (contractBytes, contractEnd) = Deser.parseArraySize(bytes, 0)
    val contractName = new String(contractBytes, "UTF-8")
    val action = ChangeContractStatusAction(bytes(contractEnd))
    val fee = Longs.fromByteArray(bytes.slice(contractEnd + 1, contractEnd + 9))
    val feeScale = Shorts.fromByteArray(bytes.slice(contractEnd + 9, contractEnd + 11))
    val timestamp = Longs.fromByteArray(bytes.slice(contractEnd + 11, contractEnd + 19))
    (for {
      proofs <- Proofs.fromBytes(bytes.slice(contractEnd + 19, bytes.length))
      tx <- ChangeContractStatusTransaction.create(contractName, action, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(contractName: String,
             action: ChangeContractStatusAction.Value,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, ChangeContractStatusTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(ChangeContractStatusTransaction(contractName, action, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             contractName: String,
             action: ChangeContractStatusAction.Value,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, ChangeContractStatusTransaction] = for {
    unsigned <- create(contractName, action, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- create(contractName, action, fee, feeScale, timestamp, proofs)
  } yield tx

  def create(sender: PublicKeyAccount,
             contractName: String,
             action: ChangeContractStatusAction.Value,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ChangeContractStatusTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- create(contractName, action, fee, feeScale, timestamp, proofs)
  } yield tx
}
