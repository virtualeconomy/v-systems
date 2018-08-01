package vee.api.http.vee

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http.assets.PaymentRequest
import scorex.transaction.{PaymentTransaction, TransactionFactory}
import scorex.utils.Time
import vee.wallet.Wallet
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.waves.transaction.SignedPaymentRequest

@Path("/vee")
@Api(value = "/vee")
case class PaymentApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time)
  extends ApiRoute with BroadcastRoute {

  override lazy val route = pathPrefix("vee") {
    payment~signPayment
  }

  @Path("/payment")
  @ApiOperation(value = "Send payment from wallet.",
    notes = "Send payment from wallet to another wallet. Each call sends new payment.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.PaymentRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def payment: Route = (path("payment") & post & withAuth) {
    json[PaymentRequest] { p =>
      doBroadcast(TransactionFactory.createPayment(p, wallet, time))
    }
  }

  @Path("/broadcast/payment")
  @ApiOperation(value = "Create payment signed by address from wallet",
    notes = "Create unique payment signed by address from wallet. Without broadcasting to network.",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.PaymentRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signPayment: Route = (post & path("broadcast" / "payment")) {
    json[PaymentRequest] { payment =>
      (for {
        sender <- wallet.findPrivateKey(payment.sender)
        recipient <- Address.fromString(payment.recipient)
        pt <- PaymentTransaction.create(sender, recipient, payment.amount, payment.fee, time.correctedTime())
      } yield pt)
        .left.map(ApiError.fromValidationError)
        .map { t =>
          SignedPaymentRequest(t.timestamp, t.amount, t.fee, t.recipient.address, Base58.encode(t.sender.publicKey), t.sender.address, t.signature.base58)
        }
    }
  }
}
