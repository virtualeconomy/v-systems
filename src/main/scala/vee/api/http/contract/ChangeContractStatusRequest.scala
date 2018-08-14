package vee.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class ChangeContractStatusRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                  sender: String,
                                  @ApiModelProperty(value = "contractName", required = true)
                                  contractName: String,
                                  @ApiModelProperty(required = true)
                                  fee: Long,
                              	  @ApiModelProperty(required = true)
                                  feeScale: Short)

object ChangeContractStatusRequest {
  implicit val changeContractStatusRequestFormat: Format[ChangeContractStatusRequest] = Json.format
}