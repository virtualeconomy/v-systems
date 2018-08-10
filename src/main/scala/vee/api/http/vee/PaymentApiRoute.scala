package vee.api.http.vee

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http.assets.PaymentRequest
import scorex.transaction.TransactionFactory
import scorex.utils.Time
import vee.wallet.Wallet
import scorex.api.http._

@Path("/vee")
@Api(value = "/vee")
case class PaymentApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time)
  extends ApiRoute with BroadcastRoute {

  override lazy val route = pathPrefix("vee") {
    payment ~ broadcastPayment
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
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"feeScale\":100,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
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
  @ApiOperation(value = "Broadcast signed payment.",
    notes = "Publish signed payment to the Blockchain.",
    httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vee.api.http.vee.SignedPaymentRequest",
      defaultValue = "{\n\t\"timestamp\": 0,\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"feeScale\":100,\n\t\"senderPublicKey\":\"senderPubKey\",\n\t\"recipient\":\"recipientId\",\n\t\"signature\":\"sig\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def broadcastPayment: Route = (path("broadcast" / "payment") & post) {
    json[SignedPaymentRequest] { payment =>
      doBroadcast(TransactionFactory.broadcastPayment(payment))
    }
  }
}
