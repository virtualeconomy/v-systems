package vsys.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ReleaseSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(required = true)
                              slotId: Int,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              @ApiModelProperty(required = true)
                              feeScale: Short)

object ReleaseSlotsRequest {
  implicit val releaseSlotsRequestFormat: Format[ReleaseSlotsRequest] = Json.format
}
