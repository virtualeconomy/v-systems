package vsys.transaction.contract

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction.{AssetId, ValidationError}
import vsys.account.ContractAccount
import vsys.contract.{Contract, DataEntry}
import vsys.transaction.proof._
import vsys.transaction.ProvenTransaction

import scala.util.{Failure, Success, Try}

case class RegisterContractTransaction private(contract: Contract,
                                               dataStack: Seq[DataEntry],
                                               description: Array[Byte],
                                               fee: Long,
                                               feeScale: Short,
                                               timestamp: Long,
                                               proofs: Proofs)
  extends ProvenTransaction {

  override val transactionType: TransactionType.Value = TransactionType.RegisterContractTransaction

  lazy val contractId: ContractAccount = ContractAccount.fromId(id)

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    BytesSerializable.arrayWithSize(contract.bytes.arr),
    Deser.serializeArray(dataStack.flatMap(_.bytes).toArray),
    BytesSerializable.arrayWithSize(description),
    Longs.toByteArray(fee),
    Shorts.toByteArray(feeScale),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "contractId" ->  contractId.address,
    "contract" -> Json.obj("languageCode" -> Base58.encode(contract.languageCode),
                                    "languageVersion" -> Base58.encode(contract.languageVersion),
                                    "descriptor" -> contract.descriptor.map(p => Base58.encode(p))),
    "dataStack" -> Base58.encode(dataStack.flatMap(_.bytes).toArray),
    "description" -> Base58.encode(description),
    "fee" -> fee,
    "feeScale" -> feeScale,
    "timestamp" -> timestamp
  )

  // TODO
  // add feeScale in assetFee, need to change 100 later
  override val assetFee: (Option[AssetId], Long, Short) = (None, fee, 100)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, proofs.bytes)

}

object RegisterContractTransaction {

  val MaxDescriptionSize = 140
  val maxDescriptionStringSize: Int = base58Length(MaxDescriptionSize)

  def parseTail(bytes: Array[Byte]): Try[RegisterContractTransaction] = Try {
    val (contractBytes, contractEnd) = Deser.parseArraySize(bytes, 0)
    (for {
      contract <- Contract.fromBytes(contractBytes)
      (dataStackBytes, dataStackEnd) = Deser.parseArraySize(bytes, contractEnd)
      dataStack = DataEntry.fromArrayBytes(dataStackBytes).right.get
      (description, descriptionEnd) = Deser.parseArraySize(bytes, dataStackEnd)
      fee = Longs.fromByteArray(bytes.slice(descriptionEnd, descriptionEnd + 8))
      feeScale = Shorts.fromByteArray(bytes.slice(descriptionEnd + 8, descriptionEnd + 10))
      timestamp = Longs.fromByteArray(bytes.slice(descriptionEnd + 10, descriptionEnd + 18))
      proofs <- Proofs.fromBytes(bytes.slice(descriptionEnd + 18, bytes.length))
      tx <- RegisterContractTransaction.createWithProof(contract, dataStack, description, fee, feeScale, timestamp, proofs)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def createWithProof(contract: Contract,
                      dataStack: Seq[DataEntry],
                      description: Array[Byte],
                      fee: Long,
                      feeScale: Short,
                      timestamp: Long,
                      proofs: Proofs): Either[ValidationError, RegisterContractTransaction] =
    if (description.length > MaxDescriptionSize) {
      Left(ValidationError.TooBigArray)
    } else if(fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (feeScale != 100) {
      Left(ValidationError.WrongFeeScale(feeScale))
    }  else {
      Right(RegisterContractTransaction(contract, dataStack, description, fee, feeScale, timestamp, proofs))
    }

  def create(sender: PrivateKeyAccount,
             contract: Contract,
             dataStack: Seq[DataEntry],
             description: Array[Byte],
             fee: Long,
             feeScale: Short,
             timestamp: Long): Either[ValidationError, RegisterContractTransaction] = for {
    unsigned <- createWithProof(contract, dataStack, description, fee, feeScale, timestamp, Proofs.empty)
    proofs <- Proofs.create(List(EllipticCurve25519Proof.createProof(unsigned.toSign, sender).bytes))
    tx <- createWithProof(contract, dataStack, description, fee, feeScale, timestamp, proofs)
  } yield tx


  def create(sender: PublicKeyAccount,
             contract: Contract,
             dataStack: Seq[DataEntry],
             description: Array[Byte],
             fee: Long,
             feeScale: Short,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, RegisterContractTransaction] = for {
    proofs <- Proofs.create(List(EllipticCurve25519Proof.buildProof(sender, signature).bytes))
    tx <- createWithProof(contract, dataStack, description, fee, feeScale, timestamp, proofs)
  } yield tx
}
