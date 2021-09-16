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
      dataType = "vsys.api.http.contract.SignedRegisterContractRequest"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
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
      dataType = "vsys.api.http.contract.SignedExecuteContractFunctionRequest"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
  def signedExecute: Route = (path("execute") & post) {
    json[SignedExecuteContractFunctionRequest] { contractReq =>
      doBroadcast(contractReq.toTx)
    }
  }

}
