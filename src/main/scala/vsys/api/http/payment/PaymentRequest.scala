package vsys.api.http.vsys

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class PaymentRequest( @ApiModelProperty(required = true)
                           amount: Long,
                           @ApiModelProperty(required = true)
                           fee: Long,
                           @ApiModelProperty(required = true)
                           feeScale: Short,
                           @ApiModelProperty(value = "Base58 encoded sender", required = true)
                           sender: String,
                           @ApiModelProperty(value = "Base58 encoded attachment")
                           attachment: Option[String],
                           @ApiModelProperty(value = "Recipient address", required = true)
                           recipient: String)

object PaymentRequest {
  val paymentReads: Reads[PaymentRequest] = (
      ((__ \ "amount").read[Long] | (__ \ "amount").read[String].map(_.toLong)) and
      (__ \ "fee").read[Long] and
      (__ \ "feeScale").read[Short] and
      (__ \ "sender").read[String] and
      (__ \ "attachment").readNullable[String] and
      (__ \ "recipient").read[String]
  )(PaymentRequest.apply _)

  val paymentWrites: Writes[PaymentRequest] = (
      (__ \ "amount").write[Long] and
      (__ \ "fee").write[Long] and
      (__ \ "feeScale").write[Short] and
      (__ \ "sender").write[String] and
      (__ \ "attachment").writeNullable[String] and
      (__ \ "recipient").write[String]
    )(unlift(PaymentRequest.unapply))

  implicit val paymentFormat: Format[PaymentRequest] = Format(paymentReads, paymentWrites)
}
