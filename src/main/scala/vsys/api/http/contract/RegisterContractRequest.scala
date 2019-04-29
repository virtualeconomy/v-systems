package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}


case class RegisterContractRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                   sender: String,
                                   @ApiModelProperty(value = "Base58 encoded contract", required = true)
                                   contract: String,
                                   @ApiModelProperty(value = "Base58 encoded dataStack", required = true)
                                   data: String,
                                   @ApiModelProperty(value = "Description of contract")
                                   description: Option[String],
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   feeScale: Short)

object RegisterContractRequest {
  implicit val registerContractRequestFormat: Format[RegisterContractRequest] = Json.format
}
