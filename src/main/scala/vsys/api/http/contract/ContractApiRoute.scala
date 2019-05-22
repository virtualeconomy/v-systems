package vsys.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.google.common.primitives.Ints
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
import vsys.account.ContractAccount.{contractIdFromBytes, tokenIdFromBytes}
import vsys.contract.ContractPermitted
import vsys.wallet.Wallet

import scala.util.Success
import scala.util.control.Exception

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    register ~ content ~ info ~ tokenInfo ~ balance ~ execute ~ tokenId
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
        case Some((h, txId, ct)) => complete(Json.obj("transactionId" -> txId.base58) ++ ct.json ++ Json.obj("height" -> JsNumber(h)))
        case None => complete(ContractNotExists)
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
        case Some((h, txId, ct)) => Right(Json.obj(
          "contractId" -> contractIdStr,
          "transactionId" -> txId.base58,
          "type" -> typeFromBytes(ct.bytes.arr),
          "info" -> JsArray((ct.stateVar, paraFromBytes(ct.textual.last)).zipped.map { (a, b) =>
            (state.contractInfo(ByteStr(id.arr ++ Array(a(0)))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2) }),
          "height" -> JsNumber(h))
        )
        case None => Left(ContractNotExists)
      }
      case _ => Left(InvalidAddress)
    }
  }

  private def typeFromBytes(bytes: Array[Byte]): String = {
    if (bytes sameElements ContractPermitted.contract.bytes.arr) {
      "TokenContractWithSplit"
    } else if (bytes sameElements ContractPermitted.contractWithoutSplit.bytes.arr) {
      "TokenContract"
    } else {
      "GeneralContract"
    }
  }

  @Path("/tokenInfo/{tokenId}")
  @ApiOperation(value = "Token's Info", notes = "Token's info by given token", httpMethod = "Get")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "tokenId", value = "Token ID", required = true, dataType = "string", paramType = "path")
  ))
  def tokenInfo: Route = (get & path("tokenInfo" / Segment)) { tokenId =>
    ByteStr.decodeBase58(tokenId) match {
      case Success(id) => {
        val maxKey = ByteStr(id.arr ++ Array(0.toByte))
        val totalKey = ByteStr(id.arr ++ Array(1.toByte))
        val unityKey = ByteStr(id.arr ++ Array(2.toByte))
        val descKey = ByteStr(id.arr ++ Array(3.toByte))
        state.tokenInfo(maxKey) match {
          case Some(x) => complete(Json.obj("tokenId" -> tokenId,
            "contractId" -> contractIdFromBytes(id.arr),
            "max" -> x.json.value("data"),
            "total" -> state.tokenAccountBalance(totalKey),
            "unity" -> state.tokenInfo(unityKey).get.json.value("data"),
            "description" -> state.tokenInfo(descKey).get.json.value("data")
          ))
          case _ => complete(TokenNotExists)
        }
      }
      case _ => complete(InvalidAddress)
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
        val unityKey = ByteStr(tokenId.arr ++ Array(2.toByte))
        state.tokenInfo(unityKey) match {
          case Some(x) => (for {
            acc <- Address.fromString(address)
          } yield Json.obj(
            "address" -> acc.address,
            "tokenId" -> tokenIdStr,
            "balance" -> state.tokenAccountBalance(ByteStr(tokenId.arr ++ acc.bytes.arr)),
            "unity" -> x.json.value("data"))
            ).left.map(ApiError.fromValidationError)
          case _ => Left(TokenNotExists)
        }
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

  @Path("contractId/{contractId}/tokenIndex/{tokenIndex}")
  @ApiOperation(value = "Token's Id", notes = "Token Id from contract Id and token index", httpMethod = "Get")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "tokenIndex", value = "Token Index", required = true, dataType = "integer", paramType = "path")
  ))
  def tokenId: Route = (pathPrefix("contractId") & get) {
    pathPrefix(Segment) { contractIdStr =>
      ByteStr.decodeBase58(contractIdStr) match {
        case Success(c) =>
          pathPrefix("tokenIndex") {
            pathEndOrSingleSlash {
              complete(InvalidTokenIndex)
            } ~
              path(Segment) { tokenIndexStr =>
                Exception.allCatch.opt(tokenIndexStr.toInt) match {
                  case Some(tokenIndex) if tokenIndex >= 0 =>
                    tokenIdFromBytes(c.arr, Ints.toByteArray(tokenIndex)) match {
                      case Right(b) => complete(Json.obj("tokenId" -> b))
                      case Left(e) => complete(e)
                    }
                  case _ =>
                    complete(InvalidTokenIndex)
                }
              }
          } ~ complete(StatusCodes.NotFound)
        case _ => complete(InvalidAddress)
      }
    }
  }

}
