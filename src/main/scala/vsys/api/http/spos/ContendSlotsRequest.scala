package vsys.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ContendSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(required = true)
                              slotId: Int,
                              @ApiModelProperty(required = true, example = "5000000000000")
                              fee: Long,
                              @ApiModelProperty(required = true, example = "100")
                              feeScale: Short)

object ContendSlotsRequest {
  implicit val contendSlotsRequestFormat: Format[ContendSlotsRequest] = Json.format
}
