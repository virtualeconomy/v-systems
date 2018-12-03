package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SignedChangeContractStatusRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                  senderPublicKey:  String,
                                  @ApiModelProperty(value = "contractName", required = true)
                                  contractName: String,
                                  @ApiModelProperty(required = true)
                                  fee: Long,
                              	  @ApiModelProperty(required = true)
                                  feeScale: Short,
                                  @ApiModelProperty(required = true)
                                  timestamp: Long,
                                  @ApiModelProperty(required = true)
                                  signature: String)

object SignedChangeContractStatusRequest {
  implicit val broadcastChangeContractStatusRequestFormat: Format[SignedChangeContractStatusRequest] = Json.format
}