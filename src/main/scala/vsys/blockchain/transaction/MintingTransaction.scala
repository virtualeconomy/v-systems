package vsys.blockchain.transaction

import java.util

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state.ByteStr
import play.api.libs.json.{JsObject, Json}
import vsys.account.Address
import vsys.utils.crypto.hash.FastCryptographicHash
import vsys.blockchain.transaction.TransactionParser._
import vsys.blockchain.consensus.SPoSCalc._

import scala.util.{DynamicVariable, Failure, Success, Try}

case class MintingTransaction private(recipient: Address,
                                      amount: Long,
                                      timestamp: Long,
                                      currentBlockHeight: Int) extends NonFeeTransaction {

  val transactionType = TransactionType.MintingTransaction

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))
  override lazy val signatureValid = true

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val currentBlockHeightBytes = Ints.toByteArray(currentBlockHeight)
    Bytes.concat(Array(transactionType.id.toByte), timestampBytes, recipient.bytes.arr, amountBytes, currentBlockHeightBytes)
  }

  override lazy val json: JsObject = Json.obj(
    "type" -> transactionType.id,
    "id" -> id.base58,
    "recipient" -> recipient.address,
    "timestamp" -> timestamp,
    "amount" -> amount,
    "currentBlockHeight" -> currentBlockHeight
  )

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign)
  
}

object MintingTransaction extends TransactionParser {

  private val recipientLength = Address.AddressLength
  private val currentBlockHeightLength = 4
  private val BaseLength = TimestampLength + recipientLength + AmountLength + currentBlockHeightLength

  def create(recipient: Address,
             amount: Long,
             timestamp: Long,
             currentBlockHeight: Int): Either[ValidationError, MintingTransaction] = {
    if (amount != MintingReward) {
      Left(ValidationError.WrongMintingReward(amount))
    } else if (Try(Math.addExact(amount, 0)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT amount won't overflow Long
    } else {
      Right(MintingTransaction(recipient, amount, timestamp, currentBlockHeight))
    }
  }

  def parseTail(data: Array[Byte]): Try[MintingTransaction] = Try {
    require(data.length >= BaseLength, "Data does not match base length")

    val positionVal = new DynamicVariable(0)
    val position = positionVal.value

    //READ TIMESTAMP
    val timestampBytes = data.take(TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    positionVal.value_=(position + TimestampLength)

    //READ RECIPIENT
    val recipientBytes = java.util.Arrays.copyOfRange(data, position, position + recipientLength)
    val recipient = Address.fromBytes(recipientBytes).right.get
    positionVal.value_=(position + recipientLength)

    //READ AMOUNT
    val amountBytes = util.Arrays.copyOfRange(data, position, position + AmountLength)
    val amount = Longs.fromByteArray(amountBytes)
    positionVal.value_=(position + AmountLength)

    //READ CURRENTBLOCKHEIGHT
    val currentBlockHeightBytes = util.Arrays.copyOfRange(data, position, position + currentBlockHeightLength)
    val currentBlockHeight = Ints.fromByteArray(currentBlockHeightBytes)
    positionVal.value_=(position + currentBlockHeightLength)

    //READ SIGNATURE
    MintingTransaction
      .create(recipient, amount, timestamp, currentBlockHeight)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

}
