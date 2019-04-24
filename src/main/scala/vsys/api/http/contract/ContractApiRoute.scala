package vsys.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.{JsArray, JsNumber, JsObject, Json}
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http._
import scorex.serialization.Deser
import scorex.transaction._
import scorex.utils.Time
import vsys.wallet.Wallet

import scala.util.Success

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    register ~ content ~ info ~ tokenInfo ~ balance ~ execute
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
      defaultValue = "{\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"contract\": \"contract\",\n\t\"data\":\"data\",\n\t\"description\":\"5VECG3ZHwy\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def register: Route = processRequest("register", (t: RegisterContractRequest) => doBroadcast(TransactionFactory.registerContract(t, wallet, time)))


  @Path("/content/{contractId}")
  @ApiOperation(value = "Contract content", notes = "Get contract content associated with a contract id.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def content: Route = (get & path("content" / Segment)) { encoded =>
    ByteStr.decodeBase58(encoded) match {
      case Success(id) => state.contractContent(id) match {
        case Some((h, _, ct)) => complete(ct.json + ("height" -> JsNumber(h)))
        case None => complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Contract is not in blockchain"))
      }
      case _ => complete(InvalidAddress)
    }
  }

  @Path("/info/{contractId}")
  @ApiOperation(value = "Info", notes = "Get contract info associated with a contract id.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def info: Route = (get & path("info" / Segment)) { contractId =>
    complete(infoJson(contractId))
  }

  private def infoJson(contractIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(contractIdStr) match {
      case Success(id) => state.contractContent(id) match {
        case Some((_, _, ct)) => Right(Json.obj(
          "contractId" -> contractIdStr,
          "info" -> JsArray((ct.stateVar, paraFromBytes(ct.textual.last)).zipped.map { (a, b) =>
            (state.contractInfo(ByteStr(id.arr ++ Array(a(0)))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2) }))
        )
        case None => Left(InvalidAddress)
      }
      case _ => Left(InvalidAddress)
    }
  }

  @Path("/tokenInfo/{tokenId}")
  @ApiOperation(value = "Token's Info", notes = "Token's info by given token", httpMethod = "Get")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tokenId", value = "Token ID", required = true, dataType = "string", paramType = "path")
  ))
  def tokenInfo: Route = (get & path("tokenInfo" / Segment)) { tokenId =>
    complete(tokenInfoJson(tokenId))
  }

  private def tokenInfoJson(tokenIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(tokenIdStr) match {
      case Success(id) => Right(Json.obj(
        "tokenId" -> tokenIdStr,
        "info" -> JsArray((Array(0), Array("max")).zipped.map {
              (a, b) => (state.tokenInfo(ByteStr(id.arr ++ Array(a.toByte))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2)} ++
              (Array(1), Array("total")).zipped.map {
                (a, b) => (state.tokenAccountBalance(ByteStr(id.arr ++ Array(a.toByte))), b) }.filter(_._1 > 0).map { a => Json.obj("data" -> a._1, "type" -> "Amount", "name" -> a._2) } ++
              (Array(2, 3), Array("unity", "description")).zipped.map {
                (a, b) => (state.tokenInfo(ByteStr(id.arr ++ Array(a.toByte))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2)})
      ))
      case _ => Left(InvalidAddress)
    }
  }

  private def paraFromBytes(bytes: Array[Byte]): Seq[String] = {
    val listParaNameBytes = Deser.parseArrays(bytes)
    listParaNameBytes.foldLeft(Seq.empty[String]) { case (e, b) => {
      val paraName = Deser.deserilizeString(b)
      e :+ paraName
    }
    }
  }

  @Path("balance/{address}/{tokenId}")
  @ApiOperation(value = "Token's balance", notes = "Account's balance by given token", httpMethod = "Get")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "tokenId", value = "Token ID", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = (get & path("balance" / Segment / Segment)) { (address, tokenId) =>
    complete(balanceJson(address, tokenId))
  }

  private def balanceJson(address: String, tokenIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(tokenIdStr) match {
      case Success(tokenId) =>
        (for {
          acc <- Address.fromString(address)
        } yield Json.obj(
          "address" -> acc.address,
          "tokenId" -> tokenIdStr,
          "balance" -> state.tokenAccountBalance(ByteStr(tokenId.arr ++ acc.bytes.arr)))
          ).left.map(ApiError.fromValidationError)
      case _ => Left(InvalidAddress)
    }
  }

  @Path("/execute")
  @ApiOperation(value = "Execute a contract function",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.contract.ExecuteContractFunctionRequest",
      defaultValue = "{\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"contractId\": \"contractId\",\n\t\"funcIdx\": \"0\",\n\t\"data\":\"data\",\n\t\"description\":\"5VECG3ZHwy\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def execute: Route = processRequest("execute", (t: ExecuteContractFunctionRequest) => doBroadcast(TransactionFactory.executeContractFunction(t, wallet, time)))

}
