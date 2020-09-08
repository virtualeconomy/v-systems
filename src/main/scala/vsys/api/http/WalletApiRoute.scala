package vsys.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import vsys.settings.RestAPISettings
import vsys.wallet.Wallet

@Path("/wallet")
@Api(value = "/wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute {

  override lazy val route = seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET",
    authorizations = Array(new Authorization("api_key")))
  def seed: Route = (path("wallet" / "seed") & get & withAuth) {
    complete(Json.obj("seed" -> wallet.seed))
  }
}
