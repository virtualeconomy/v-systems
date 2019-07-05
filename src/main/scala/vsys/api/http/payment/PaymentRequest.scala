package vsys.api.http.payment

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class PaymentRequest(@ApiModelProperty(required = true)
                          amount: Long,
                          @ApiModelProperty(required = true, example = "10000000")
                          fee: Long,
                          @ApiModelProperty(required = true, example = "100")
                          feeScale: Short,
                          @ApiModelProperty(value = "Base58 encoded sender address", required = true)
                          sender: String,
                          @ApiModelProperty(value = "Base58 encoded attachment")
                          attachment: Option[String],
                          @ApiModelProperty(value = "Base58 encoded recipient address", required = true)
                          recipient: String)

object PaymentRequest {
  implicit val paymentFormat: Format[PaymentRequest] = Json.format
}
