package vsys.api.http.vsys

import com.wavesplatform.state2.ByteStr
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Address, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.{PaymentTransaction, ValidationError}
import scorex.transaction.assets.TransferTransaction

@ApiModel(value = "Signed Payment transaction")
case class SignedPaymentRequest( @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(required = true)
                                 amount: Long,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(required = true)
                                 feeScale: Short,
                                 @ApiModelProperty(value = "Recipient address", required = true)
                                 recipient: String,
                                 @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 senderPublicKey: String,
                                 @ApiModelProperty(value = "Base58 encoded attachment")
                                 attachment: Option[String],
                                 @ApiModelProperty(required = true)
                                 signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, PaymentTransaction] =
    for {
      _signature <- ByteStr.decodeBase58(signature).toOption.toRight(ValidationError.InvalidRequestSignature)
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _recipient <- Address.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      t <- PaymentTransaction.create(_sender, _recipient, amount, fee, feeScale, timestamp, _attachment.arr, _signature)
    } yield t
}

object SignedPaymentRequest {
  val broadcastPaymentReads: Reads[SignedPaymentRequest] = (
      (__ \ "timestamp").read[Long] and
      ((__ \ "amount").read[Long] | (__ \ "amount").read[String].map(_.toLong)) and
      (__ \ "fee").read[Long] and
      (__ \ "feeScale").read[Short] and
      (__ \ "recipient").read[String] and
      (__ \ "senderPublicKey").read[String] and
      (__ \ "attachment").readNullable[String] and
      (__ \ "signature").read[String]
    )(SignedPaymentRequest.apply _)

  val broadcastPaymentWrites: Writes[SignedPaymentRequest] = (
      (__ \ "timestamp").write[Long] and
      (__ \ "amount").write[Long] and
      (__ \ "fee").write[Long] and
      (__ \ "feeScale").write[Short] and
      (__ \ "recipient").write[String] and
      (__ \ "senderPublicKey").write[String] and
      (__ \ "attachment").writeNullable[String] and
      (__ \ "signature").write[String]
    )(unlift(SignedPaymentRequest.unapply))

  implicit val broadcastPaymentFormat: Format[SignedPaymentRequest] = Format(broadcastPaymentReads, broadcastPaymentWrites)
}
