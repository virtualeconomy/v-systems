package vee.api.http.spos

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._

@Path("/spos/broadcast")
@Api(value = "/spos")
case class SPOSBroadcastApiRoute(
    settings: RestAPISettings,
    utx: UtxPool,
    allChannels: ChannelGroup)
  extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("spos" / "broadcast") {
    signedContend ~ signRelease
  }

  @Path("/contend")
  @ApiOperation(value = "Broadcasts a signed contend transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.spos.SignedContendSlotsRequest",
      defaultValue = "{\n\t\"slotid\": 0,\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000\n\t\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedContend: Route = (path("contend") & post) {
    json[SignedContendSlotsRequest] { contendReq =>
      doBroadcast(contendReq.toTx)
    }
  }

  @Path("/release")
  @ApiOperation(value = "Broadcasts a signed release transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.spos.SignedReleaseSlotsRequest",
      defaultValue = "{\n\t\"slotid\": 0,\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000\n\t\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signRelease: Route = (path("release") & post) {
    json[SignedReleaseSlotsRequest] { releaseReq =>
      doBroadcast(releaseReq.toTx)
    }
  }
}
