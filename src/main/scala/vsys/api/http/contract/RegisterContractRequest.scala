package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}


case class RegisterContractRequest(@ApiModelProperty(value = "Base58 encoded sender address", required = true)
                                   sender: String,
                                   @ApiModelProperty(value = "Base58 encoded contract", required = true)
                                   contract: String,
                                   @ApiModelProperty(value = "Base58 encoded init data", required = true)
                                   initData: String,
                                   @ApiModelProperty(value = "Description of contract")
                                   description: Option[String],
                                   @ApiModelProperty(required = true, example = "10000000000")
                                   fee: Long,
                                   @ApiModelProperty(required = true, example = "100")
                                   feeScale: Short)

object RegisterContractRequest {
  implicit val registerContractRequestFormat: Format[RegisterContractRequest] = Json.format
}
