package vsys.consensus.spos.api.http

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.account.Address
import scorex.api.http.{ApiRoute, CommonApiFunctions, InvalidAddress, InvalidSlotId}
import scorex.transaction.{History, PoSCalc}
import vsys.spos.SPoSCalc

@Path("/consensus")
@Api(value = "/consensus")
case class SposConsensusApiRoute(
    settings: RestAPISettings,
    state: StateReader,
    history: History,
    fs:FunctionalitySettings) extends ApiRoute with CommonApiFunctions {

  override val route: Route =
    pathPrefix("consensus") {
      algo ~ allSlotsInfo ~ mintingAverageBalance ~ mintingAverageBalanceId ~ mintTime ~ mintTimeId ~ generatingBalance
    }

  @Path("/generatingBalance/{address}")
  @ApiOperation(value = "Generating balance", notes = "Account's generating balance(the same as balance atm)", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def generatingBalance: Route = (path("generatingBalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(InvalidAddress)
      case Right(account) =>
        complete(Json.obj(
          "address" -> account.address,
          "balance" -> PoSCalc.generatingBalance(state, fs, account, state.height)))
    }
  }

  @Path("/mintingAverageBalance/{address}")
  @ApiOperation(value = "Minting average balance", notes = "Account's minting average balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def mintingAverageBalance: Route = (path("mintingAverageBalance" / Segment) & get) { address =>
    Address.fromString(address) match {
      case Left(_) => complete(InvalidAddress)
      case Right(account) =>
        complete(Json.obj(
          "address" -> account.address,
          "mintingAverageBalance" -> SPoSCalc.mintingBalance(state, fs, account, state.height),
          "height" -> state.height))
    }
  }

  @Path("/allSlotsInfo")
  @ApiOperation(value = "Get all slots' info", notes = "Get all slots' information", httpMethod = "GET")
  def allSlotsInfo: Route = (path("allSlotsInfo") & get) {
    val h = state.height
    val ret = Json.arr(Json.obj("height" -> h)) ++ JsArray(
      (0 until fs.numOfSlots).map{
        f => state.slotAddress(f) match {
          case None => Json.obj(
            "slotId"-> f,
            "address" -> "None",
            "mintingAverageBalance" -> 0)
          case Some(address) => Address.fromString(address) match {
            case Left(_) => Json.obj(
              "slotId"-> f,
              "address" -> "Error address",
              "mintingAverageBalance" -> 0)
            case Right(account) =>
              Json.obj(
                "slotId"-> f,
                "address" -> account.address,
                "mintingAverageBalance" -> SPoSCalc.mintingBalance(state, fs, account, h))
          }
        }
      }
    )
    complete(ret)
  }

  @Path("/slotInfo/{slotId}")
  @ApiOperation(value = "Minting average balance with slot ID", notes = "Account of supernode's minting average balance", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "slotId", value = "Slot Id", required = true, dataType = "integer", paramType = "path")
  ))
  def mintingAverageBalanceId: Route = (path("slotInfo" / IntNumber) & get) { slotId =>
    state.slotAddress(slotId) match {
      case None if slotId >= 0 && slotId < fs.numOfSlots =>
        complete(Json.obj(
          "slotId"-> slotId,
          "address" -> "None",
          "mintingAverageBalance" -> 0,
          "height" -> state.height))
      case Some(address) =>
        Address.fromString(address) match {
          case Left(_) => complete(InvalidAddress)
          case Right(account) =>
            complete(Json.obj(
              "slotId"-> slotId,
              "address" -> account.address,
              "mintingAverageBalance" -> SPoSCalc.mintingBalance(state, fs, account, state.height),
              "height" -> state.height))
        }
      case _ => complete(InvalidSlotId(slotId))
    }
  }

  @Path("/mintTime/{blockId}")
  @ApiOperation(value = "Mint time", notes = "Mint time of a block with specified id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "blockId", value = "Block id ", required = true, dataType = "string", paramType = "path")
  ))
  def mintTimeId: Route = (path("mintTime" / Segment) & get) { encodedSignature =>
    withBlock(history, encodedSignature) { block =>
      complete(Json.obj("mintTime" -> block.consensusData.mintTime))
    }
  }

  @Path("/mintTime")
  @ApiOperation(value = "Mint time last", notes = "Mint time of a last block", httpMethod = "GET")
  def mintTime: Route = (path("mintTime") & get) {
    complete(Json.obj("mintTime" -> history.lastBlock.get.consensusData.mintTime))
  }

  @Path("/algo")
  @ApiOperation(value = "Consensus algo", notes = "Shows which consensus algo being using", httpMethod = "GET")
  def algo: Route = (path("algo") & get) {
    complete(Json.obj("consensusAlgo" -> "supernode-proof-of-stake (SPoS)"))
  }
}
