package vee.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ContendSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(required = true)
                              slotId: Int,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              @ApiModelProperty(required = true)
                              feeScale: Short)

object ContendSlotsRequest {
  implicit val aliasRequestFormat: Format[ContendSlotsRequest] = Json.format
}
