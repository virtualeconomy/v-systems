package vsys.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction.TransactionFactory
import vsys.transaction.contract.ChangeContractStatusAction

@Path("/contract/broadcast")
@Api(value = "/contract")
case class ContractBroadcastApiRoute(
                                   settings: RestAPISettings,
                                   utx: UtxPool,
                                   allChannels: ChannelGroup)
  extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("contract" / "broadcast") {
    signedCreate ~ signedEnable ~ signedDisable
  }

  @Path("/create")
  @ApiOperation(value = "Broadcasts a signed create conract transaction",
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
  def signedCreate: Route = (path("create") & post) {
    json[SignedRegisterContractRequest] { contractReq =>
      doBroadcast(contractReq.toTx)
    }
  }

  @Path("/enable")
  @ApiOperation(value = "Broadcasts a change contract status transaction (enable)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.SignedChangeContractStatusRequest",
      defaultValue = "{\n\t\"contractName\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100,\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedEnable: Route = (path("enable") & post) {
    json[SignedChangeContractStatusRequest] { enableContract =>
      doBroadcast(TransactionFactory.broadcastChangeContractStatus(enableContract, ChangeContractStatusAction.Enable))
    }
  }

  @Path("/disable")
  @ApiOperation(value = "Broadcasts a change contract status transaction (disable)",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.SignedChangeContractStatusRequest",
      defaultValue = "{\n\t\"contractName\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100,\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedDisable: Route = (path("disable") & post) {
    json[SignedChangeContractStatusRequest] { disableContract =>
      doBroadcast(TransactionFactory.broadcastChangeContractStatus(disableContract, ChangeContractStatusAction.Disable))
    }
  }

}
