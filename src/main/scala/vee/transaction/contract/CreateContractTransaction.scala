package vee.transaction.contract

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import vee.contract.Contract
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}

import scala.util.{Failure, Success, Try}

case class CreateContractTransaction private(sender: PublicKeyAccount,
                                          contract: Contract,
                                          fee: Long,
                                          feeScale: Short,
                                          timestamp: Long,
                                          signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.CreateContractTransaction

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(transactionType.id.toByte +: contract.name.getBytes("UTF-8")))

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    BytesSerializable.arrayWithSize(contract.bytes.arr),
    Longs.toByteArray(fee),
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
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object CreateContractTransaction {

  def parseTail(bytes: Array[Byte]): Try[CreateContractTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (contractBytes, contractEnd) = Deser.parseArraySize(bytes, KeyLength)
    (for {
      contract <- Contract.fromBytes(contractBytes)
      fee = Longs.fromByteArray(bytes.slice(contractEnd, contractEnd + 8))
      feeScale = Shorts.fromByteArray(bytes.slice(contractEnd + 8, contractEnd + 10))
      timestamp = Longs.fromByteArray(bytes.slice(contractEnd + 10, contractEnd + 18))
      signature = ByteStr(bytes.slice(contractEnd + 18, contractEnd + 18 + SignatureLength))
      tx <- CreateContractTransaction.create(sender, contract, fee, feeScale, timestamp, signature)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             contract: Contract,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, CreateContractTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(CreateContractTransaction(sender, contract, fee, feeScale, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             contract: Contract,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, CreateContractTransaction] = {
    create(sender, contract, fee, feeScale, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
