package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}


case class ExecuteContractFunctionRequest(@ApiModelProperty(value = "Base58 encoded sender address", required = true)
                                          sender: String,
                                          @ApiModelProperty(value = "Base58 encoded contract id", required = true)
                                          contractId: String,
                                          @ApiModelProperty(required = true)
                                          functionIndex: Short,
                                          @ApiModelProperty(value = "Base58 encoded function data", required = true)
                                          functionData: String,
                                          @ApiModelProperty(value = "Base58 encoded attachment of contract")
                                          attachment: Option[String],
                                          @ApiModelProperty(required = true)
                                          fee: Long,
                                          @ApiModelProperty(required = true)
                                          feeScale: Short)

object ExecuteContractFunctionRequest {
  implicit  val executeContractFunctionRequestFormat: Format[ExecuteContractFunctionRequest] = Json.format
}