package scorex.transaction

import java.util

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class MintingTransaction private(sender: PublicKeyAccount,
                                      amount: Long,
                                      fee: Long,
                                      timestamp: Long,
                                      signature: ByteStr) extends SignedTransaction {
  override val transactionType = TransactionType.MintingTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)
  val minterAddress: Address = sender.toAddress

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)
    Bytes.concat(Array(transactionType.id.toByte), timestampBytes, sender.publicKey, amountBytes, feeBytes)
  }

  override lazy val id: ByteStr= ByteStr(FastCryptographicHash(toSign))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "minterPublicKey" -> Base58.encode(sender.publicKey),
      "minterAddress" -> minterAddress.address,
      "amount" -> amount)

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

}

object MintingTransaction {

  val mintingFee = 100000
  val mintingReward = 100000000

  private val minterLength = 32
  private val FeeLength = 8
  private val BaseLength = TimestampLength + minterLength + AmountLength + FeeLength + SignatureLength

  def create(minter: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, MintingTransaction] = {
    create(minter, amount, fee, timestamp, ByteStr.empty).right.map(unsigned => {
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(minter, unsigned.toSign)))
    })
  }

  def create(minter: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, MintingTransaction] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee) //CHECK IF FEE IS POSITIVE
    } else if (Try(Math.addExact(amount, 0)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else {
      Right(MintingTransaction(minter, amount, fee, timestamp, signature))
    }
  }

  def parseTail(data: Array[Byte]): Try[MintingTransaction] = Try {
    require(data.length >= BaseLength, "Data does not match base length")

    var position = 0

    //READ TIMESTAMP
    val timestampBytes = data.take(TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TimestampLength

    //READ MINTER
    val minterBytes = util.Arrays.copyOfRange(data, position, position + minterLength)
    val minter = PublicKeyAccount(minterBytes)
    position += minterLength

    //READ AMOUNT
    val amountBytes = util.Arrays.copyOfRange(data, position, position + AmountLength)
    val amount = Longs.fromByteArray(amountBytes)
    position += AmountLength

    //READ FEE
    val feeBytes = util.Arrays.copyOfRange(data, position, position + FeeLength)
    val fee = Longs.fromByteArray(feeBytes)
    position += FeeLength

    //READ SIGNATURE
    val signatureBytes = util.Arrays.copyOfRange(data, position, position + SignatureLength)

    MintingTransaction
      .create(minter, amount, fee, timestamp, ByteStr(signatureBytes))
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

}
