package vsys.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import vsys.settings.RestAPISettings
import vsys.utils.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import vsys.utils.crypto.hash.{FastCryptographicHash, SecureCryptographicHash}
import vsys.utils.Time

@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(timeService: Time, settings: RestAPISettings) extends ApiRoute {
  import UtilsApiRoute._

  private def seed(length: Int) = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashFast ~ hashSecure ~ sign ~ time
  }

  @Path("/time")
  @ApiOperation(value = "Time", notes = "Get the Current Node time (UTC)", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json response of the current node time or error")
    ))
  def time: Route = (path("time") & get) {
    val t = System.currentTimeMillis()*1000000L + System.nanoTime()%1000000L
    complete(Json.obj("system" -> t, "NTP" -> timeService.correctedTime()))
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of a random seed or error")
  ))
  def seedRoute: Route = (path("seed") & get) {
    complete(seed(DefaultSeedSize))
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified `length`", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "integer", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of a random seed or error")
  ))
  def length: Route = (path("seed" / IntNumber) & get) { length =>
      if (length <= MaxSeedSize) complete(seed(length))
      else complete(TooBigArrayAllocation)
  }

  @Path("/hash/secure")
  @ApiOperation(value = "Hash", notes = "Return a hash(SecureCryptographicHash) of a specified `message`", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  def hashSecure: Route = (path("hash" / "secure") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(SecureCryptographicHash(message))))
    }
  }

  @Path("/hash/fast")
  @ApiOperation(value = "Hash", notes = "Return a hash(FastCryptographicHash) of a specified `message`", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  def hashFast: Route = (path("hash" / "fast") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(FastCryptographicHash(message))))
    }
  }


  @Path("/sign/{privateKey}")
  @ApiOperation(value = "Hash", notes = "Return a hash(FastCryptographicHash) of a specified message with the `privateKey`", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "privateKey", value = "privateKey", required = true, paramType = "path", dataType = "string"),
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  def sign: Route = (path("sign" / Segment) & post) {pk =>
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "signature" ->
        Base58.encode(EllipticCurveImpl.sign(Base58.decode(pk).get, Base58.decode(message).get))))
    }
  }
}

object UtilsApiRoute {
  val MaxSeedSize = 1024
  val DefaultSeedSize = 32
}
