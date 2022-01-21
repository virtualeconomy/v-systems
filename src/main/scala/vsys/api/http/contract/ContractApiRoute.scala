package vsys.api.http.contract

import javax.ws.rs.Path
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.google.common.primitives.Ints
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.{Format, JsArray, JsNumber, JsObject, Json}
import vsys.account.{Account, ContractAccount}
import vsys.account.ContractAccount.{contractIdFromBytes, tokenIdFromBytes, tokenIndexFromBytes}
import vsys.api.http._
import vsys.blockchain.state.ByteStr
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction._
import vsys.blockchain.UtxPool
import vsys.blockchain.contract._
import vsys.settings.RestAPISettings
import vsys.utils.serialization.Deser
import vsys.utils.Time
import vsys.wallet.Wallet

import scala.util.Success
import scala.util.control.Exception
import ContractApiRoute._

@Path("/contract")
@Api(value = "/contract")
case class ContractApiRoute (settings: RestAPISettings,
                             wallet: Wallet,
                             utx: UtxPool,
                             allChannels: ChannelGroup,
                             time: Time,
                             state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("contract") {
    register ~ content ~ info ~ tokenInfo ~ balance ~ execute ~ tokenId ~ vBalance ~ getContractData ~ lastToken
  }

  @Path("/register")
  @ApiOperation(value = "Register a contract",
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
      dataType = "vsys.api.http.contract.RegisterContractRequest",
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
  def register: Route = processRequest("register", (t: RegisterContractRequest) => doBroadcast(TransactionFactory.registerContract(t, wallet, time)))

  @Path("/vBalance/{contractId}")
  @ApiOperation(value = "Contract balance", notes = "Get contract account **v balance** associated with `contractId`.", httpMethod = "GET", response = classOf[Balance])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def vBalance: Route = (path("vBalance" / Segment) & get) { contractId =>
    complete(vBalanceJson(contractId))
  }

  private def vBalanceJson(contractId: String): ToResponseMarshallable = {
    Account.fromString(contractId).right.map(acc => ToResponseMarshallable(Balance(
      acc.bytes.base58,
      0,
      state.balance(acc)
    ))).getOrElse(InvalidContractAddress)
  }

  @Path("/lastTokenIndex/{contractId}")
  @ApiOperation(value = "Last Token Index", notes = "Token contract last token index", httpMethod = "Get")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response with Last Token Index or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def lastToken: Route = (get & path("lastTokenIndex" / Segment)) { contractId =>
    ByteStr.decodeBase58(contractId) match {
      case Success(id) if ContractAccount.fromString(contractId).isRight => {
        val lastTokenIndex = state.contractTokens(id) - 1
        if (lastTokenIndex == -1) {
          complete(CustomValidationError("No token generated in this contract"))
        } else {
          complete(TokenIndex(contractId, lastTokenIndex))
        }
      }
      case _ => complete(InvalidContractAddress)
    }
  }

  @Path("/content/{contractId}")
  @ApiOperation(value = "Contract content", notes = "Get **contract content** associated with a `contractId`.", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of the contract content or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def content: Route = (get & path("content" / Segment)) { encoded =>
    ByteStr.decodeBase58(encoded) match {
      case Success(id) if id != ContractAccount.systemContractId.bytes => state.contractContent(id) match {
        case Some((h, txId, ct)) => complete(Json.obj("transactionId" -> txId.base58) ++ ct.json ++ Json.obj("height" -> JsNumber(h)))
        case None => complete(ContractNotExists)
      }
      case Success(id) if id == ContractAccount.systemContractId.bytes => complete(Json.obj("transactionId" -> "") ++ ContractSystem.contract.json ++ Json.obj("height" -> JsNumber(-1)))
      case _ => complete(InvalidAddress)
    }
  }

  @Path("/data/{contractId}/{key}")
  @ApiOperation(value = "Contract Data", notes = "Get **contract data** by given `contractId` and `key` (default numerical 0).", httpMethod = "Get", authorizations = Array(new Authorization("api_key")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of contract data or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract Account", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "key", value = "Key", required = true, dataType = "string", paramType = "path")
  ))
  def getContractData: Route = (get & path("data" / Segment / Segment)) { (contractId, key) =>
    complete(dataJson(contractId, key))
  }

  @Path("/info/{contractId}")
  @ApiOperation(value = "Info", notes = "Get **contract info** associated with a `contractId`.", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of the contract info or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "contractId", value = "Contract ID", required = true, dataType = "string", paramType = "path")
  ))
  def info: Route = (get & path("info" / Segment)) { contractId =>
    complete(infoJson(contractId))
  }

  private def infoJson(contractIdStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(contractIdStr) match {
      case Success(id) if id != ContractAccount.systemContractId.bytes => state.contractContent(id) match {
        case Some((h, txId, ct)) => Right(Json.obj(
          "contractId" -> contractIdStr,
          "transactionId" -> txId.base58,
          "type" -> typeFromBytes(ct.bytes),
          "info" -> JsArray((ct.stateVar, paraFromBytes(ct.textual(2))).zipped.map { (a, b) =>
            (state.contractInfo(ByteStr(id.arr ++ Array(a(0)))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2) }),
          "height" -> JsNumber(h))
        )
        case None => Left(ContractNotExists)
      }
      case Success(id) if id == ContractAccount.systemContractId.bytes => Right(Json.obj(
        "contractId" -> contractIdStr,
        "transactionId" -> "",
        "type" -> typeFromBytes(ContractSystem.contract.bytes),
        "info" -> JsArray((ContractSystem.contract.stateVar, paraFromBytes(ContractSystem.contract.textual(2))).zipped.map { (a, b) =>
          (state.contractInfo(ByteStr(id.arr ++ Array(a(0)))), b) }.filter(_._1.isDefined).map { a => a._1.get.json ++ Json.obj("name" -> a._2) }),
        "height" -> JsNumber(-1))
      )
      case _ => Left(InvalidAddress)
    }
  }

  private val contractType: Map[ByteStr, String] = Map(
    ContractPermitted.contract.bytes -> "TokenContractWithSplit",
    ContractPermitted.contractWithoutSplit.bytes -> "TokenContract",
    ContractSystem.contract.bytes -> "SystemContract",
    ContractLock.contract.bytes -> "LockContract",
    ContractNonFungible.contract.bytes -> "NonFungibleContract",
    ContractPaymentChannel.contract.bytes -> "PaymentChannelContract",
    ContractAtomicSwap.contract.bytes -> "AtomicSwapContract",
    ContractVSwap.contract.bytes -> "VSwapContract",
    ContractVOption.contract.bytes -> "VOptionContract",
    ContractVStableSwap.contract.bytes -> "VStableSwapContract",
    ContractVEscrow.contract.bytes -> "ESCROWContract",
    ContractTokenV2.contractTokenWhiteList.bytes -> "TokenContractWithWhitelist",
    ContractTokenV2.contractTokenBlackList.bytes -> "TokenContractWithBlacklist",
    ContractNonFungibleV2.contractNFTWhitelist.bytes -> "NFTContractWithWhitelist",
    ContractNonFungibleV2.contractNFTBlacklist.bytes -> "NFTContractWithBlacklist"
  )

  private def typeFromBytes(bytes: ByteStr): String =
    contractType.getOrElse(bytes, "GeneralContract")

  @Path("/tokenInfo/{tokenId}")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of token info or error")
  ))
  @ApiOperation(value = "Token's Info", notes = "Get the **token's info** by given `tokenId`", httpMethod = "Get")
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
            "description" -> state.tokenInfo(descKey).get.json.value("data"),
            "tokenIndex" -> tokenIndexFromBytes(id.arr)
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

  @Path("/balance/{address}/{tokenId}")
  @ApiOperation(value = "Token's balance", notes = "Get the **balance** of a specified `tokenId` by a given `address`", httpMethod = "Get")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of the token balance or error")
  ))
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
        val height = state.height
        state.tokenInfo(unityKey) match {
          case Some(x) => (for {
            acc <- Account.fromString(address)
          } yield Json.obj(
            "address/contractId" -> acc.bytes.base58,
            "height" -> height,
            "tokenId" -> tokenIdStr,
            "balance" -> state.tokenAccountBalance(ByteStr(tokenId.arr ++ acc.bytes.arr)),
            "unity" -> x.json.value("data"))
            ).left.map(ApiError.fromValidationError)
          case _ => Left(TokenNotExists)
        }
      case _ => Left(InvalidAddress)
    }
  }

  private def dataJson(contractIdStr: String, keyStr: String): Either[ApiError, JsObject] = {
    ByteStr.decodeBase58(contractIdStr) match {
      case Success(contractId) =>
        ByteStr.decodeBase58(keyStr) match {
          case Success(key) =>
            val dataKey = ByteStr(contractId.arr ++ key.arr)
            state.contractInfo(dataKey) match {
              case Some(x) => Right(Json.obj("contractId" -> contractIdStr,
                                      "key" -> keyStr,
                                      "height" -> state.height,
                                      "dbName" -> "contractInfo",
                                      "dataType" -> x.json.value("type"),
                                       "value" -> x.json.value("data")))
              case _ => Right(Json.obj("contractId" -> contractIdStr,
                                 "key" -> keyStr,
                                 "height" -> state.height,
                                 "dbName" -> "contractNumInfo",
                                 "value" -> state.contractNumInfo(dataKey)))
            }
          case _ => Left(InvalidDbKey)
        }
      case _ => Left(InvalidContractAddress)
    }
  }

  @Path("/execute")
  @ApiOperation(value = "Execute a contract function",
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
      dataType = "vsys.api.http.contract.ExecuteContractFunctionRequest"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
  def execute: Route = processRequest("execute", (t: ExecuteContractFunctionRequest) => doBroadcast(TransactionFactory.executeContractFunction(t, wallet, time)))

  @Path("/contractId/{contractId}/tokenIndex/{tokenIndex}")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of a token id or error")
  ))
  @ApiOperation(value = "Token's Id", notes = "Get the **token Id** from the specified `contractId` and `tokenIndex`", httpMethod = "Get")
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

object ContractApiRoute {

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class TokenIndex(contractId: String, lastTokenIndex: Int)

  implicit val tokenIndexFormat: Format[TokenIndex] = Json.format

}