package vsys.blockchain.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state.ByteStr
import play.api.libs.json.{JsObject, Json}
import vsys.account.Address
import vsys.utils.crypto.hash.FastCryptographicHash._
import vsys.utils.crypto.hash.FastCryptographicHash
import vsys.blockchain.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class GenesisTransaction private(recipient: Address, amount: Long, slotId: Int, timestamp: Long, signature: ByteStr) extends Transaction {

  import GenesisTransaction._

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val slotIdBytes = Ints.toByteArray(slotId)

    Bytes.concat(Array(transactionType.id.toByte),
      timestampBytes,
      amountBytes,
      slotIdBytes,
      recipient.bytes.arr)
  }
  override val assetFee: (Option[AssetId], Long, Short) = (None, 0, 100)
  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))

  val transactionType = TransactionType.GenesisTransaction

  lazy val creator: Option[Address] = None

  lazy val json: JsObject =
    Json.obj("type" -> transactionType.id,
      "id" -> id.base58,
      "fee" -> 0,
      "slotId" -> slotId,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "recipient" -> recipient.address,
      "amount" -> amount)

  lazy val bytes: Array[Byte] = {
    val typeBytes = Array(transactionType.id.toByte)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val slotIdBytes = Bytes.ensureCapacity(Ints.toByteArray(slotId), SlotIdLength, 0)
    val rcpBytes = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes, slotIdBytes)
    require(res.length == TypeLength + BASE_LENGTH)
    res
  }

  override lazy val signatureValid: Boolean = true
}


object GenesisTransaction extends {

  private val RECIPIENT_LENGTH = Address.AddressLength
  private val BASE_LENGTH = TimestampLength + RECIPIENT_LENGTH + AmountLength + SlotIdLength

  def generateSignature(recipient: Address, amount: Long, slotId: Int, timestamp: Long): Array[Byte] = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GenesisTransaction.id), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes = Longs.toByteArray(amount)
    val amountFill = new Array[Byte](AmountLength - amountBytes.length)
    val slotIdBytes = Bytes.ensureCapacity(Ints.toByteArray(slotId), SlotIdLength, 0)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes), slotIdBytes)

    val h = hash(data)
    Bytes.concat(h, h)
  }


  def parseTail(data: Array[Byte]): Try[GenesisTransaction] =
    Try {
      require(data.length >= BASE_LENGTH, "Data does not match base length")

      var position = 0

      val timestampBytes = java.util.Arrays.copyOfRange(data, position, position + TimestampLength)
      val timestamp = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      val recipientBytes = java.util.Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
      val recipient = Address.fromBytes(recipientBytes).right.get
      position += RECIPIENT_LENGTH

      val amountBytes = java.util.Arrays.copyOfRange(data, position, position + AmountLength)
      val amount = Longs.fromByteArray(amountBytes)
      position += AmountLength

      val slotIdBytes = java.util.Arrays.copyOfRange(data, position, position + SlotIdLength)
      val slotId = Ints.fromByteArray(slotIdBytes)

      GenesisTransaction.create(recipient, amount, slotId, timestamp).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(recipient: Address, amount: Long, slotId: Int, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(ValidationError.NegativeAmount)
    } else {
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, slotId, timestamp))
      Right(GenesisTransaction(recipient, amount, slotId, timestamp, signature))
    }
  }
}
