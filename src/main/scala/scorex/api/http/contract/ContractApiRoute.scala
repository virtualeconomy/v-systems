package scorex.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    create ~ contentFromName
  }

  @Path("/create")
  @ApiOperation(value = "Creates a contract",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.contract.CreateContractRequest",
      defaultValue = "{\n\t\"contract\": \"contractcontract\",\n\t\"name\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def create: Route = processRequest("create", (t: CreateContractRequest) => doBroadcast(TransactionFactory.contract(t, wallet, time)))


  @Path("/by-name/{name}")
  @ApiOperation(value = "Content", notes = "Returns content associated with a contract name.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "name", value = "Name", required = true, dataType = "string", paramType = "path")
  ))
  def contentFromName: Route = (get & path("by-name" / Segment)) { contractName =>
    val content = state.contractContent(contractName)
    val result = Either.cond(content.isDefined, Json.obj("content" -> content.get), ContractNotExists(contractName))
    complete(result)
  }
}
