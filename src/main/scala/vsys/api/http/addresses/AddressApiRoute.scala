package vsys.api.http.addresses

import java.nio.charset.StandardCharsets

import javax.ws.rs.Path
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.crypto.encode.Base58
import vsys.account.{Account, Address, PublicKeyAccount}
import vsys.api.http._
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.consensus.SPoSCalc
import vsys.settings.{FunctionalitySettings, RestAPISettings}
import vsys.utils.crypto.EllipticCurveImpl
import vsys.wallet.Wallet

import scala.util.{Failure, Success, Try}

@Path("/addresses")
@Api(value = "/addresses/", description = "Info about wallet's accounts and other calls about addresses")
case class AddressApiRoute(settings: RestAPISettings, wallet: Wallet, state: StateReader, functionalitySettings: FunctionalitySettings) extends ApiRoute {
  import AddressApiRoute._

  val MaxAddressesPerRequest = 1000

  override lazy val route =
    pathPrefix("addresses") {
      validate ~ seed ~ balanceWithConfirmations ~ balanceDetails ~ balance ~ balanceWithConfirmations ~ verify ~ sign ~ deleteAddress ~ verifyText ~
        signText ~ seq ~ publicKey ~ effectiveBalance ~ effectiveBalanceWithConfirmations
    } ~ root ~ create

  @Path("/{address}")
  @ApiOperation(value = "Delete", notes = "Remove the `address` from the wallet", httpMethod = "DELETE",
    authorizations = Array(new Authorization("api_key")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def deleteAddress: Route = path(Segment) { address =>
    (delete & withAuth) {
      if (Address.fromString(address).isLeft) {
        complete(InvalidAddress)
      } else {
        val deleted = wallet.findPrivateKey(address).exists(account =>
          wallet.deleteAccount(account))
        complete(Json.obj("deleted" -> deleted))
      }
    }
  }

  @Path("/sign/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a `message` with a private key associated with an `address`", httpMethod = "POST",
    authorizations = Array(new Authorization("api_key")))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  def sign: Route = {
    path("sign" / Segment) { address =>
      signPath(address, encode = true)
    }
  }

  @Path("/signText/{address}")
  @ApiOperation(value = "Sign", notes = "Sign a message with a private key associated with `address`", httpMethod = "POST",
    authorizations = Array(new Authorization("api_key")))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to sign as a plain string", required = true, paramType = "body", dataType = "string"),
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  def signText: Route = {
    path("signText" / Segment) { address =>
      signPath(address, encode = false)
    }
  }

  @Path("/verify/{address}")
  @ApiOperation(value = "Verify", notes = "Check a signature of a base58-encoded message(`body`) signed by an `address`", httpMethod = "POST",
    authorizations = Array(new Authorization("api_key")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.addresses.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Base58-encoded message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verify: Route = {
    path("verify" / Segment) { address =>
      verifyPath(address, decode = true)
    }
  }

  @Path("/verifyText/{address}")
  @ApiOperation(value = "Verify text", notes = "Check a signature of a plain message(`body`) signed by an `address`", httpMethod = "POST",
    authorizations = Array(new Authorization("api_key")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.addresses.SignedMessage",
      defaultValue = "{\n\t\"message\":\"Plain message\",\n\t\"signature\":\"Base58-encoded signature\",\n\t\"publickey\":\"Base58-encoded public key\"\n}"
    )
  ))
  def verifyText: Route = path("verifyText" / Segment) { address =>
    verifyPath(address, decode = false)
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Balance", notes = "Get the available balance of a specified `address`", httpMethod = "GET", response = classOf[Balance])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = (path("balance" / Segment) & get) { address =>
    complete(balanceJson(address))
  }

  @Path("/balance/details/{address}")
  @ApiOperation(value = "Details for balance", notes = "Get the balance details of a specified `address` including **effective, mintingAverage and available balance**.", httpMethod = "GET", response = classOf[BalanceDetails])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balanceDetails: Route = (path("balance" / "details" / Segment) & get) { address =>
    complete(Address.fromString(address).right.map(acc => {
      ToResponseMarshallable(balancesDetailsJson(acc))
    }).getOrElse(InvalidAddress))
  }

  @Path("/balance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Get the balance of an `address` after `confirmations`", httpMethod = "GET", response = classOf[Balance])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
  ))
  def balanceWithConfirmations: Route = {
    (path("balance" / Segment / IntNumber) & get) { case (address, confirmations) =>
      complete(balanceJson(address, confirmations))
    }
  }


  @Path("/effectiveBalance/{address}")
  @ApiOperation(value = "Balance", notes = "Get the effective balance of a specified `address`", httpMethod = "GET", response = classOf[Balance])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def effectiveBalance: Route = {
    path("effectiveBalance" / Segment) { address =>
      complete(effectiveBalanceJson(address, 0))
    }
  }

  @Path("/effectiveBalance/{address}/{confirmations}")
  @ApiOperation(value = "Confirmed balance", notes = "Get the balance of an `address` after `confirmations`", httpMethod = "GET", response = classOf[Balance])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "confirmations", value = "0", required = true, dataType = "integer", paramType = "path")
  ))
  def effectiveBalanceWithConfirmations: Route = {
    path("effectiveBalance" / Segment / IntNumber) { case (address, confirmations) =>
      complete(
        effectiveBalanceJson(address, confirmations)
      )
    }
  }

  @Path("/seed/{address}")
  @ApiOperation(value = "Seed", notes = "Export seed value for the `address`", httpMethod = "GET",
    authorizations = Array(new Authorization("api_key")))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of a seed or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def seed: Route = {
    (path("seed" / Segment) & get & withAuth) { address =>
      complete(for {
        pk <- wallet.findPrivateKey(address)
        seed <- wallet.exportAccountSeed(pk)
      } yield Json.obj("address" -> address, "seed" -> Base58.encode(seed))
      )
    }
  }

  @Path("/validate/{address}")
  @ApiOperation(value = "Validate", notes = "Check whether `address` is valid or not", httpMethod = "GET", response = classOf[Validity])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def validate: Route = (path("validate" / Segment) & get) { address =>
    complete(Validity(address, Address.fromString(address).isRight))
  }

  @Path("/")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of a addresses list or error")
  ))
  @ApiOperation(value = "Addresses", notes = "Get wallet accounts addresses", httpMethod = "GET")
  def root: Route = (path("addresses") & get) {
    val accounts = wallet.privateKeyAccounts
    val json = JsArray(accounts.map(a => JsString(a.address)))
    complete(json)
  }

  @Path("/seq/{from}/{to}")
  @ApiOperation(value = "Seq", notes = "Get wallet accounts addresses by specified a start nonce index(`from`) and a end nonce index(`to`)", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of wallet addresses or error")
  ))
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "from", value = "nonce start index", required = true, dataType = "integer", paramType = "path"),
    new ApiImplicitParam(name = "to", value = "nonce end index", required = true, dataType = "integer", paramType = "path")
  ))
  def seq: Route = {
    (path("seq" / IntNumber / IntNumber) & get) { case (start, end) =>
      if (start >= 0 && end >= 0 && start - end < MaxAddressesPerRequest) {
        val json = JsArray(
          wallet.privateKeyAccounts.map(a => JsString(a.address)).slice(start, end)
        )

        complete(json)
      } else complete(TooBigArrayAllocation)
    }
  }

  @Path("/")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  @ApiOperation(value = "Create", notes = "Create a new account in the wallet(if it exists)", httpMethod = "POST",
    authorizations = Array(new Authorization("api_key")))
  def create: Route = (path("addresses") & post) {
    withAuth {
      wallet.generateNewAccount() match {
        case Some(pka) => complete(Json.obj("address" -> pka.address))
        case None => complete(Unknown)
      }
    }
  }

  private def balanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    Address.fromString(address).right.map(acc => ToResponseMarshallable(Balance(
      acc.address,
      confirmations,
      state.balanceWithConfirmations(acc, confirmations)
    ))).getOrElse(InvalidAddress)
  }

  private def balanceJson(address: String): ToResponseMarshallable = {
    Account.fromString(address).right.map(acc => ToResponseMarshallable(Balance(
      acc.bytes.base58,
      0,
      state.balance(acc)
    ))).getOrElse(InvalidAddress)
  }

  private def balancesDetailsJson(account: Address): BalanceDetails = {
    state.read { _ =>
      val portfolio = state.accountPortfolio(account)
      BalanceDetails(
        account.address,
        portfolio.balance,
        SPoSCalc.mintingBalance(state, functionalitySettings, account, state.height),
        portfolio.balance - portfolio.leaseInfo.leaseOut,
        state.effectiveBalance(account),
        state.height)
    }
  }

  private def effectiveBalanceJson(address: String, confirmations: Int): ToResponseMarshallable = {
    state.read { _ =>
      Address.fromString(address).right.map(acc => ToResponseMarshallable(Balance(
        acc.address,
        confirmations,
        state.effectiveBalanceAtHeightWithConfirmations(acc, state.height, confirmations))))
        .getOrElse(InvalidAddress)
    }
  }

  private def signPath(address: String, encode: Boolean) = (post & entity(as[String])) { message =>
    withAuth {
      val res = wallet.findPrivateKey(address).map(pk => {
        val messageBytes = message.getBytes(StandardCharsets.UTF_8)
        val signature = EllipticCurveImpl.sign(pk, messageBytes)
        val msg = if (encode) Base58.encode(messageBytes) else message
        Signed(msg, Base58.encode(pk.publicKey), Base58.encode(signature))
      })
      complete(res)
    }
  }

  private def verifyPath(address: String, decode: Boolean) = withAuth {
    json[SignedMessage] { m =>
      if (Address.fromString(address).isLeft) {
        InvalidAddress
      } else {
        //DECODE SIGNATURE
        val msg: Try[Array[Byte]] = if (decode) Base58.decode(m.message) else Success(m.message.getBytes)
        verifySigned(msg, m.signature, m.publickey, address)
      }
    }
  }

  private def verifySigned(msg: Try[Array[Byte]], signature: String, publicKey: String, address: String) = {
    (msg, Base58.decode(signature), Base58.decode(publicKey)) match {
      case (Success(msgBytes), Success(signatureBytes), Success(pubKeyBytes)) =>
        val account = PublicKeyAccount(pubKeyBytes)
        val isValid = account.address == address && EllipticCurveImpl.verify(signatureBytes, msgBytes, pubKeyBytes)
        Right(Json.obj("valid" -> isValid))
      case _ => Left(InvalidMessage)
    }
  }

  @Path("/publicKey/{publicKey}")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "publicKey", value = "Public key Base58-encoded", required = true,
      paramType = "path", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json response of an address or error")
  ))
  @ApiOperation(value = "Address from Public Key", notes = "Generate an address from a `publicKey`", httpMethod = "GET")
  def publicKey: Route = (path("publicKey" / Segment) & get) { publicKey =>
    Base58.decode(publicKey) match {
      case Success(pubKeyBytes) => {
        val account = Address.fromPublicKey(pubKeyBytes)
        complete(Json.obj("address" -> account.address))
      }
      case Failure(_) => complete(InvalidPublicKey)
    }
  }
}

object AddressApiRoute {

  case class Signed(message: String, publicKey: String, signature: String)

  implicit val signedFormat: Format[Signed] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class BalanceDetails(address: String, regular: Long, mintingAverage: Long, available: Long, effective: Long, height: Int)

  implicit val balanceDetailsFormat: Format[BalanceDetails] = Json.format

  case class Validity(address: String, valid: Boolean)

  implicit val validityFormat: Format[Validity] = Json.format
}
