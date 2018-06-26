package scorex.consensus.nxt.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.account.Address
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidAddress}
import scorex.crypto.encode.Base58
import scorex.transaction.{History, PoSCalc}

@Path("/consensus")
@Api(value = "/consensus")
case class NxtConsensusApiRoute(
    settings: RestAPISettings,
    state: StateReader,
    history: History,
    fs:FunctionalitySettings) extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ minttime ~ minttimeId ~ generationSignature ~ generationSignatureId ~ generatingBalance
    }

  @Path("/generatingbalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def generatingBalance: Route = (path("generatingbalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(InvalidAddress)
      case Right(account) =>
        complete(Json.obj(
          "address" -> account.address,
          "balance" -> PoSCalc.generatingBalance(state, fs, account, state.height)))
    }
  }

  @Path("/generationsignature/{blockId}")
  @ApiOperation(value = "Generation signature", notes = "Generation signature of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def generationSignatureId: Route = (path("generationsignature" / Segment) & get) { encodedSignature =>
    withBlock(history, encodedSignature) { block =>
      complete(Json.obj("generationSignature" -> Base58.encode(block.consensusData.generationSignature)))
    }
  }

  @Path("/generationsignature")
  @ApiOperation(value = "Generation signature last", notes = "Generation signature of a last block", httpMethod = "GET")
  def generationSignature: Route = (path("generationsignature") & get) {
    complete(Json.obj("generationSignature" -> Base58.encode(history.lastBlock.get.consensusData.generationSignature)))
  }

  @Path("/minttime/{blockId}")
  @ApiOperation(value = "Mint time", notes = "Mint time of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def minttimeId: Route = (path("minttime" / Segment) & get) { encodedSignature =>
    withBlock(history, encodedSignature) { block =>
      complete(Json.obj("mintTime" -> block.consensusData.mintTime))
    }
  }

  @Path("/minttime")
  @ApiOperation(value = "Mint time last", notes = "Mint time of a last block", httpMethod = "GET")
  def minttime: Route = (path("minttime") & get) {
    complete(Json.obj("mintTime" -> history.lastBlock.get.consensusData.mintTime))
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = (path("algo") & get) {
    complete(Json.obj("consensusAlgo" -> "proof-of-stake (PoS)"))
  }
}
