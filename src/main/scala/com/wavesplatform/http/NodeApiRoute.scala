package com.wavesplatform.http

import java.time.Instant

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.Shutdownable
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.settings.{Constants, RestAPISettings}
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.transaction.History
import scorex.utils.ScorexLogging

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings,
                        s: StateReader,
                        history: History,
                        application: Shutdownable)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "Get VSYS node version", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json VSYS node version")
  ))
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the node", httpMethod = "POST")
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core", httpMethod = "GET")
  def status: Route = (get & path("status")) {
    val lastUpdated = history.lastBlock.get.timestamp
    complete(
      Json.obj(
        "blockchainHeight" -> history.height,
        "stateHeight"      -> s.height,
        "updatedTimestamp" -> lastUpdated,
        "updatedDate"      -> Instant.ofEpochMilli(lastUpdated/1000000L).toString
      ))
  }
}
