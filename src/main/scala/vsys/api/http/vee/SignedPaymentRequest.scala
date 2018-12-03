package vsys.api.http.vsys

import com.wavesplatform.state2.ByteStr
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
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
  implicit val broadcastPaymentFormat: Format[SignedPaymentRequest] = Json.format
}
