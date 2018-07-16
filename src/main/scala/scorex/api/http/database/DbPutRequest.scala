package scorex.api.http.database

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class DbPutRequest (@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                         sender: String,
                         @ApiModelProperty(value = "name", required = true)
                         name: String,
                         @ApiModelProperty(value = "dataType", required = true)
                         dataType: String,
                         @ApiModelProperty(value = "data")
                         data: String,
                         @ApiModelProperty(required = true)
                         fee: Long)

object DbPutRequest {
  implicit val dbPutRequestFormat: Format[DbPutRequest] = Json.format
}
