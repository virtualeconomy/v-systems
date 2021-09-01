package vsys.api.http.payment

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import vsys.api.http._
import vsys.blockchain.transaction.TransactionFactory
import vsys.blockchain.UtxPool
import vsys.settings.RestAPISettings
import vsys.utils.Time
import vsys.wallet.Wallet

@Path("/vsys")
@Api(value = "/vsys")
case class PaymentApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time)
  extends ApiRoute with BroadcastRoute {

  override lazy val route = pathPrefix("vsys") {
    payment ~ broadcastPayment
  }

  @Path("/payment")
  @ApiOperation(value = "Send payment from wallet.",
    notes = "Send payment from wallet to another wallet.",
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
      dataType = "vsys.api.http.payment.PaymentRequest"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
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
      dataType = "vsys.api.http.payment.SignedPaymentRequest"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
  def broadcastPayment: Route = (path("broadcast" / "payment") & post) {
    json[SignedPaymentRequest] { payment =>
      doBroadcast(payment.toTx)
    }
  }
}
