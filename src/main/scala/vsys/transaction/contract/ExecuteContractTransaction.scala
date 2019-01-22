package vsys.transaction.contract

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.hash.FastCryptographicHash.DigestSize
//import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction.{AssetId, ValidationError}
import vsys.transaction.proof._
import vsys.transaction.ProvenTransaction

import scala.util.{Failure, Success, Try}

case class ExecuteContractTransaction (contractId: ByteStr,
                                       //contractEntryPoint: Array[Short],
                                       //dataStack: DataStack,
                                       fee: Long,
                                       feeScale: Short,
                                       timestamp: Long,
                                       proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ExecuteContractTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp),
    contractId.arr
    //BytesSerializable.arrayWithSize(contractEntryPoint.map(_.toByte)),
    //BytesSerializable.arrayWithSize(dataStack.arr)
  )

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp,
    "contractId" -> contractId
  )

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object ExecuteContractTransaction {

  def parseTail(bytes: Array[Byte]): Try[ExecuteContractTransaction] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, 8))
    val feeScale = Shorts.fromByteArray(bytes.slice(8, 10))
    val timestamp = Longs.fromByteArray(bytes.slice(10, 18))
    val contractId = ByteStr(bytes.slice(18, 18 + DigestSize))
    //val (contractEntryPointBytes, ss) = Deser.parseArraySize(bytes, 18 + DigestSize)
    //val contractEntryPoint = contractEntryPointBytes.map(_.toShort)
    //val (dataStack, sss) = Deser.parseArraySize(bytes, ss)
    (for {
      proofs <- Proofs.fromBytes(bytes.slice(18 + DigestSize, bytes.length))
      tx <- ExecuteContractTransaction.createWithProof(contractId, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(contractId: ByteStr,
                      //contractEntryPoint: Array[Short],
                      //dataStack: DataStack,
                      fee: Long,
                      feeScale: Short,
                      timestamp: Long,
                      proofs: Proofs): Either[ValidationError, ExecuteContractTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(ExecuteContractTransaction(contractId, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             contractId: ByteStr,
             //contractEntryPoint: Array[Short],
             //dataStack: DataStack,
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, ExecuteContractTransaction] = for {
    unsigned <- createWithProof(contractId, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(contractId, fee, feeScale, timestamp, proofs)
  } yield tx

  def create(sender: PublicKeyAccount,
             contractId: ByteStr,
             //contractEntryPoint: Array[Short],
             //dataStack: DataStack,
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ExecuteContractTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(contractId, fee, feeScale, timestamp, proofs)
  } yield tx
}
