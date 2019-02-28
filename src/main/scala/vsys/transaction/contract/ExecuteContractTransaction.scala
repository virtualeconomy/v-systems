package vsys.transaction.contract

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import vsys.contract.DataEntry
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction.{AssetId, ValidationError}
import scorex.crypto.encode.Base58
import vsys.account.ContractAccount
import vsys.transaction.proof._
import vsys.transaction.ProvenTransaction

import scala.util.{Failure, Success, Try}

case class ExecuteContractTransaction (contractId: ContractAccount,
                                       entryPoints: Seq[Int],
                                       dataStack: Seq[DataEntry],
                                       description: Array[Byte],
                                       fee: Long,
                                       feeScale: Short,
                                       timestamp: Long,
                                       proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ExecuteContractTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    contractId.bytes.arr,
    Deser.serializeArray(Deser.serializeInts(entryPoints)),
    Deser.serializeArray(dataStack.flatMap(_.bytes).toArray),
    BytesSerializable.arrayWithSize(description),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp)
  )

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "contractId" -> contractId.address,
    "entryPoints" -> entryPoints,
    "dataStack" -> Base58.encode(dataStack.flatMap(_.bytes).toArray),
    "description" -> Base58.encode(description),
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp
  )

  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, feeScale)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object ExecuteContractTransaction {

  val MaxDescriptionSize = 140
  val maxDescriptionStringSize: Int = base58Length(MaxDescriptionSize)

  def parseTail(bytes: Array[Byte]): Try[ExecuteContractTransaction] = Try {
    val contractId = ContractAccount.fromBytes(bytes.slice(0, ContractAccount.AddressLength)).right.get
    val (entryPointsBytes, entryPointsEnd) = Deser.parseArraySize(bytes, ContractAccount.AddressLength)
    val entryPoints = Deser.deserializeInts(entryPointsBytes)
    val (dataStackBytes, dataStackEnd) = Deser.parseArraySize(bytes, entryPointsEnd)
    val dataStack = DataEntry.fromArrayBytes(dataStackBytes).right.get
    val (description, descriptionEnd) = Deser.parseArraySize(bytes, dataStackEnd)
    val fee = Longs.fromByteArray(bytes.slice(descriptionEnd, descriptionEnd + 8))
    val feeScale = Shorts.fromByteArray(bytes.slice(descriptionEnd + 8, descriptionEnd + 10))
    val timestamp = Longs.fromByteArray(bytes.slice(descriptionEnd + 10, descriptionEnd + 18))
    (for {
      proofs <- Proofs.fromBytes(bytes.slice(descriptionEnd + 18, bytes.length))
      tx <- ExecuteContractTransaction.createWithProof(contractId, entryPoints,
        dataStack, description, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(contractId: ContractAccount,
                      entryPoints: Seq[Int],
                      dataStack: Seq[DataEntry],
                      description: Array[Byte],
                      fee: Long,
                      feeScale: Short,
                      timestamp: Long,
                      proofs: Proofs): Either[ValidationError, ExecuteContractTransaction] =
    if (description.length > MaxDescriptionSize) {
      Left(ValidationError.TooBigArray)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    } else {
      Right(ExecuteContractTransaction(contractId, entryPoints, dataStack, description, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             contractId: ContractAccount,
             entryPoints: Seq[Int],
             dataStack: Seq[DataEntry],
             description: Array[Byte],
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, ExecuteContractTransaction] = for {
    unsigned <- createWithProof(contractId, entryPoints, dataStack, description, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(contractId, entryPoints, dataStack, description, fee, feeScale, timestamp, proofs)
  } yield tx

  def create(sender: PublicKeyAccount,
             contractId: ContractAccount,
             entryPoints: Seq[Int],
             dataStack: Seq[DataEntry],
             description: Array[Byte],
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ExecuteContractTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(contractId, entryPoints, dataStack, description, fee, feeScale, timestamp, proofs)
  } yield tx
}
