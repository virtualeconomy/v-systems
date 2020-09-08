package vsys.api.http.contract

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import vsys.api.http._
import vsys.blockchain.UtxPool
import vsys.settings.RestAPISettings


@Path("/contract/broadcast")
@Api(value = "/contract")
case class ContractBroadcastApiRoute(settings: RestAPISettings,
                                     utx: UtxPool,
                                     allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("contract" / "broadcast") {
    signedRegister ~ signedExecute
  }

  @Path("/register")
  @ApiOperation(value = "Broadcasts a signed register contract transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.SignedRegisterContractRequest",
      defaultValue = "{\n\t\"contract\": \"contract\",\n\t\"data\":\"data\",\n\t\"description\":\"5VECG3ZHwy\",\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100,\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedRegister: Route = (path("register") & post) {
    json[SignedRegisterContractRequest] { contractReq =>
      doBroadcast(contractReq.toTx)
    }
  }

  @Path("/execute")
  @ApiOperation(value = "Broadcasts a signed execute contract function transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.SignedExecuteContractFunctionRequest",
      defaultValue = "{\n\t\"contractId\": \"contractId\",\n\t\"funcIdx\": \"0\",\n\t\"data\":\"data\",\n\t\"description\":\"5VECG3ZHwy\",\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100,\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedExecute: Route = (path("execute") & post) {
    json[SignedExecuteContractFunctionRequest] { contractReq =>
      doBroadcast(contractReq.toTx)
    }
  }

}
