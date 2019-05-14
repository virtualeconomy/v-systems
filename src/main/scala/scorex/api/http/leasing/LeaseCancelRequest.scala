package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class LeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                              txId: String,
                              @ApiModelProperty(required = true, example = "10000000")
                              fee: Long,
                              @ApiModelProperty(required = true, example = "100")
                              feeScale: Short)

object LeaseCancelRequest {
  implicit val leaseCancelRequestFormat: Format[LeaseCancelRequest] = Json.format
}
