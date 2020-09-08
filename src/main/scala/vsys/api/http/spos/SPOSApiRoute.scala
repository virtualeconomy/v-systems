package vsys.api.http.spos

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import vsys.api.http._
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction._
import vsys.blockchain.UtxPool
import vsys.settings.RestAPISettings
import vsys.utils.Time
import vsys.wallet.Wallet

@Path("/spos")
@Api(value = "/spos")
case class SPOSApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("spos") {
    contend ~ release
  }

  @Path("/contend")
  @ApiOperation(value = "Contend a slot",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    authorizations = Array(new Authorization("api_key")))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.spos.ContendSlotsRequest",
      defaultValue = "{\n\t\"slotId\": 0,\n\t\"sender\": \"3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5\",\n\t\"fee\": 10000000\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def contend: Route = processRequest("contend", (t: ContendSlotsRequest) => doBroadcast(TransactionFactory.contendSlots(t, wallet, time)))

  @Path("/release")
  @ApiOperation(value = "Release a slot",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json",
    authorizations = Array(new Authorization("api_key")))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.spos.ReleaseSlotsRequest",
      defaultValue = "{\n\t\"slotId\": 0,\n\t\"sender\": \"3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5\",\n\t\"fee\": 100000\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def release: Route = processRequest("release", (t: ReleaseSlotsRequest) => doBroadcast(TransactionFactory.releaseSlots(t, wallet, time)))

}
