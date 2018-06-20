package scorex.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateContractRequest (@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             sender: String,
                             @ApiModelProperty(value = "Name", required = true)
                             name: String,
                             @ApiModelProperty(value = "Content", required = true)
                             content: String,
                             @ApiModelProperty(required = true)
                             fee: Long)

object CreateContractRequest {
  implicit val contractRequestFormat: Format[CreateContractRequest] = Json.format
}
