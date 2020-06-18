package vsys.api.http.payment

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import monix.execution.Scheduler
import vsys.api.http._
import vsys.blockchain.transaction.TransactionFactory
import vsys.blockchain.UtxPool
import vsys.settings.RestAPISettings
import vsys.utils.Time
import vsys.wallet.Wallet

@Path("/vsys")
@Api(value = "/vsys")
case class PaymentApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, limitedScheduler: Scheduler)
  extends ApiRoute with BroadcastRoute {

  override lazy val route = pathPrefix("vsys") {
    payment ~ broadcastPayment
  }

  @Path("/payment")
  @ApiOperation(value = "Send payment from wallet.",
    notes = "Send payment from wallet to another wallet. Each call sends new payment.",
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
      dataType = "vsys.api.http.payment.PaymentRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"feeScale\":100,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\",\n\t\"attachment\":\"5VECG3ZHwy\"\n}"
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
      dataType = "vsys.api.http.payment.SignedPaymentRequest",
      defaultValue = "{\n\t\"timestamp\": 0,\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"feeScale\":100,\n\t\"senderPublicKey\":\"senderPubKey\",\n\t\"recipient\":\"recipientId\",\n\t\"attachment\":\"5VECG3ZHwy\",\n\t\"signature\":\"sig\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def broadcastPayment: Route = (path("broadcast" / "payment") & post) {
    json[SignedPaymentRequest] { payment =>
      doBroadcastWithLimit(payment.toTx, limitedScheduler)
    }
  }
}
