package vsys.api.http.contract

import akka.http.scaladsl.model.StatusCodes
import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.{JsNumber, Json}
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction._
import vsys.transaction.contract.ChangeContractStatusAction
import scorex.utils.Time
import vsys.wallet.Wallet

import scala.util.Success

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    register ~ contentFromId ~ enable ~ disable
  }

  @Path("/register")
  @ApiOperation(value = "Register a contract",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.RegisterContractRequest",
      defaultValue = "{\n\t\"contract\": \"contractcontract\",\n\t\"name\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def register: Route = processRequest("register", (t: RegisterContractRequest) => doBroadcast(TransactionFactory.registerContract(t, wallet, time)))


  @Path("/info/{contractId}")
  @ApiOperation(value = "Info", notes = "Get contract info associated with a contract id.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "contract id", required = true, dataType = "string", paramType = "path")
  ))
  def contentFromId: Route = (get & path("info" / Segment)) { encoded =>
    ByteStr.decodeBase58(encoded) match {
      case Success(id) => state.contractContent(id) match {
        case Some((h, ct)) => complete(ct.json + ("height" -> JsNumber(h)))
        case None => complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Contract is not in blockchain"))
      }
      case _ => complete(InvalidAddress)
    }
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
      dataType = "vsys.api.http.contract.ChangeContractStatusRequest",
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
      dataType = "vsys.api.http.contract.ChangeContractStatusRequest",
      defaultValue = "{\n\t\"contractName\": \"contractname\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def disable: Route = processRequest("disable", (t: ChangeContractStatusRequest) => doBroadcast(TransactionFactory.changeContractStatus(t, ChangeContractStatusAction.Disable, wallet, time)))

}
