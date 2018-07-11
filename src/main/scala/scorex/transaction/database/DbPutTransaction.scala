package scorex.transaction.database

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.database.Entry
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}

import scala.util.{Failure, Success, Try}

case class DbPutTransaction private(sender: PublicKeyAccount,
                                    name: String,
                                    entry: Entry,
                                    fee: Long,
                                    timestamp: Long,
                                    signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.DbPutTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    BytesSerializable.arrayWithSize(name.getBytes("UTF-8")),
    BytesSerializable.arrayWithSize(entry.bytes.arr),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "name" -> name,
    "entry" -> entry.json,
    "fee" -> fee,
    "timestamp" -> timestamp
  )

  lazy val storageKey: ByteStr = DbPutTransaction.generateKey(sender.toAddress, name)
  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object DbPutTransaction {

  def generateKey(owner: Address, key: String):ByteStr =
    ByteStr(owner.bytes.arr ++ key.getBytes("UTF-8"))

  def parseTail(bytes: Array[Byte]): Try[DbPutTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (nameBytes, nameEnd) = Deser.parseArraySize(bytes, KeyLength)
    val (dbEntryBytes, dbEntryEnd) = Deser.parseArraySize(bytes, nameEnd)
    (for {
      dbEntry <- Entry.fromBytes(dbEntryBytes)
      fee = Longs.fromByteArray(bytes.slice(dbEntryEnd, dbEntryEnd + 8))
      timestamp = Longs.fromByteArray(bytes.slice(dbEntryEnd + 8, dbEntryEnd + 16))
      signature = ByteStr(bytes.slice(dbEntryEnd + 16, dbEntryEnd + 16 + SignatureLength))
      tx <- DbPutTransaction.create(sender, new String(nameBytes, "UTF-8"), dbEntry, fee, timestamp, signature)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             dbKey: String,
             dbEntry: Entry,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, DbPutTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DbPutTransaction(sender, dbKey, dbEntry, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             name: String,
             entry: Entry,
             fee: Long,
             timestamp: Long): Either[ValidationError, DbPutTransaction] = {
    create(sender, name, entry, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}