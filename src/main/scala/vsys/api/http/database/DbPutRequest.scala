package vsys.api.http.database

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class DbPutRequest (@ApiModelProperty(value = "Base58 encoded sender address", required = true)
                         sender: String,
                         @ApiModelProperty(value = "name", required = true)
                         dbKey: String,
                         @ApiModelProperty(value = "dataType", required = true)
                         dataType: String,
                         @ApiModelProperty(value = "data")
                         data: String,
                         @ApiModelProperty(required = true, example = "100000000")
                         fee: Long,
                         @ApiModelProperty(required = true, example = "100")
                         feeScale: Short)

object DbPutRequest {
  implicit val dbPutRequestFormat: Format[DbPutRequest] = Json.format
}
