package scorex.api.http.database

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class DbGetRequest (@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                         sender: String,
                         @ApiModelProperty(value = "Base58 encoded dbOwner public key", required = true)
                         nameSpace: String,
                         @ApiModelProperty(value = "key", required = true)
                         key: String,
                         @ApiModelProperty(required = true)
                         fee: Long)

object DbGetRequest {
  implicit val dbGetRequestFormat: Format[DbGetRequest] = Json.format
}
