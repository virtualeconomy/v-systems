package vsys.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._


@Path("/contract/broadcast")
@Api(value = "/contract")
case class ContractBroadcastApiRoute(settings: RestAPISettings,
                                     utx: UtxPool,
                                     allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("contract" / "broadcast") {
    signedRegister
  }

  @Path("/register")
  @ApiOperation(value = "Broadcasts a signed create contract transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.SignedCreateContractRequest",
      defaultValue = "{\n\t\"content\": \"contractcontent\",\n\t\"name\": \"contractname\",\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100,\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedRegister: Route = (path("register") & post) {
    json[SignedRegisterContractRequest] { contractReq =>
      doBroadcast(contractReq.toTx)
    }
  }

}
