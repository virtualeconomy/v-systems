package vsys.transaction.contract

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import vsys.contract.Contract
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction.{AssetId, ValidationError}
import vsys.transaction.proof._
import vsys.transaction.ProvenTransaction

import scala.util.{Failure, Success, Try}

case class CreateContractTransaction private(contract: Contract,
                                          fee: Long,
                                          feeScale: Short,
                                          timestamp: Long,
                                          proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.CreateContractTransaction

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(transactionType.id.toByte +: contract.name.getBytes("UTF-8")))

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    BytesSerializable.arrayWithSize(contract.bytes.arr),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "contract" -> (Json.obj("name" -> contract.name, "content"->contract.content, "enabled"->contract.enabled)),
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp
  )

  // TODO
  // add feeScale in assetFee, need to change 100 later
  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, 100)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object CreateContractTransaction {

  def parseTail(bytes: Array[Byte]): Try[CreateContractTransaction] = Try {
    val (contractBytes, contractEnd) = Deser.parseArraySize(bytes, 0)
    (for {
      contract <- Contract.fromBytes(contractBytes)
      fee = Longs.fromByteArray(bytes.slice(contractEnd, contractEnd + 8))
      feeScale = Shorts.fromByteArray(bytes.slice(contractEnd + 8, contractEnd + 10))
      timestamp = Longs.fromByteArray(bytes.slice(contractEnd + 10, contractEnd + 18))
      proofs <- Proofs.fromBytes(bytes.slice(contractEnd + 18, bytes.length))
      tx <- CreateContractTransaction.create(contract, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(contract: Contract,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, CreateContractTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    }  else {
      Right(CreateContractTransaction(contract, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             contract: Contract,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, CreateContractTransaction] = for {
    unsigned <- create(contract, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- create(contract, fee, feeScale, timestamp, proofs)
  } yield tx


  def create(sender: PublicKeyAccount,
             contract: Contract,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, CreateContractTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- create(contract, fee, feeScale, timestamp, proofs)
  } yield tx
}
