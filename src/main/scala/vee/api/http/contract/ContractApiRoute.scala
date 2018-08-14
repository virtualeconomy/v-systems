package vee.api.http.contract

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
import vee.transaction.contract.ChangeContractStatusAction
import scorex.utils.Time
import vee.wallet.Wallet

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    create ~ contentFromName ~ enable ~ disable
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
      dataType = "vee.api.http.contract.CreateContractRequest",
      defaultValue = "{\n\t\"contract\": \"contractcontract\",\n\t\"name\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def create: Route = processRequest("create", (t: CreateContractRequest) => doBroadcast(TransactionFactory.createContract(t, wallet, time)))


  @Path("/by-name/{name}")
  @ApiOperation(value = "Content", notes = "Returns content associated with a contract name.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "name", value = "Name", required = true, dataType = "string", paramType = "path")
  ))
  def contentFromName: Route = (get & path("by-name" / Segment)) { contractName =>
    val content = state.contractContent(contractName)
    val result = Either.cond(content.isDefined, Json.obj("content" -> content.get._3, "enabled" -> content.get._1)
      , ContractNotExists(contractName))
    complete(result)
  }

  @Path("/enable")
  @ApiOperation(value = "Enables a contract",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vee.api.http.contract.ChangeContractStatusRequest",
      defaultValue = "{\n\t\"contractName\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def enable: Route = processRequest("enable", (t: ChangeContractStatusRequest) => doBroadcast(TransactionFactory.changeContractStatus(t, ChangeContractStatusAction.Enable, wallet, time)))

  @Path("/disable")
  @ApiOperation(value = "Disacbles a contract",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vee.api.http.contract.ChangeContractStatusRequest",
      defaultValue = "{\n\t\"contractName\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def disable: Route = processRequest("disable", (t: ChangeContractStatusRequest) => doBroadcast(TransactionFactory.changeContractStatus(t, ChangeContractStatusAction.Disable, wallet, time)))

}
