package vee.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ReleaseSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(required = true)
                              slotids: Int,
                              @ApiModelProperty(required = true)
                              fee: Long)

object ReleaseSlotsRequest {
  implicit val aliasRequestFormat: Format[ReleaseSlotsRequest] = Json.format
}
