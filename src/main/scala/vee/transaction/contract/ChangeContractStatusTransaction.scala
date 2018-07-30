package vee.transaction.contract

import java.lang.String

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}

import scala.util.{Failure, Success, Try}

case class ChangeContractStatusTransaction private(sender: PublicKeyAccount,
                                              contractName: String,
                                              action: ChangeContractStatusAction.Value,
                                              fee: Long,
                                              timestamp: Long,
                                              signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.ChangeContractStatusTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    BytesSerializable.arrayWithSize(contractName.getBytes("UTF-8")),
    Array(action.id.toByte),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "contract" -> contractName,
    "fee" -> fee,
    "action" -> action.toString(),
    "timestamp" -> timestamp
  )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)
}

object ChangeContractStatusTransaction {

  def parseTail(bytes: Array[Byte]): Try[ChangeContractStatusTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (contractBytes, contractEnd) = Deser.parseArraySize(bytes, KeyLength)
    val contractName = new String(contractBytes, "UTF-8")
    val action = ChangeContractStatusAction(bytes(contractEnd))
    val fee = Longs.fromByteArray(bytes.slice(contractEnd + 1, contractEnd + 9))
    val timestamp = Longs.fromByteArray(bytes.slice(contractEnd + 9, contractEnd + 17))
    val signature = ByteStr(bytes.slice(contractEnd + 17, contractEnd + 17 + SignatureLength))
    val tx:Either[ValidationError, ChangeContractStatusTransaction] = for {
      tx <- ChangeContractStatusTransaction.create(sender, contractName, action, fee, timestamp, signature)
    } yield tx
    tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             contractName: String,
             action: ChangeContractStatusAction.Value,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, ChangeContractStatusTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(ChangeContractStatusTransaction(sender, contractName, action, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             contractName: String,
             action: ChangeContractStatusAction.Value,
             fee: Long,
             timestamp: Long): Either[ValidationError, ChangeContractStatusTransaction] = {
    create(sender, contractName, action, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
