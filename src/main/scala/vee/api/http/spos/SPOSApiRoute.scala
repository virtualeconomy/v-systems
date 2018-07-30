package vee.api.http.spos

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction._
import scorex.utils.Time
import vee.wallet.Wallet

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
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.spos.ContendSlotsRequest",
      defaultValue = "{\n\t\"slotids\": 0,\n\t\"sender\": \"3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5\",\n\t\"fee\": 10000000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def contend: Route = processRequest("contend", (t: ContendSlotsRequest) => doBroadcast(TransactionFactory.contendSlots(t, wallet, time)))

  @Path("/release")
  @ApiOperation(value = "Release a slot",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.spos.ReleaseSlotsRequest",
      defaultValue = "{\n\t\"slotids\": 0,\n\t\"sender\": \"3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def release: Route = processRequest("release", (t: ReleaseSlotsRequest) => doBroadcast(TransactionFactory.releaseSlots(t, wallet, time)))

}
