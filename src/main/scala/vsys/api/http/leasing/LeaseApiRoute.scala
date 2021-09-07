package vsys.api.http.leasing

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import vsys.api.http._
import vsys.api.http.leasing.LeaseCancelRequest.leaseCancelRequestFormat
import vsys.api.http.leasing.LeaseRequest.leaseRequestFormat
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction._
import vsys.blockchain.UtxPool
import vsys.settings.RestAPISettings
import vsys.utils.Time
import vsys.wallet.Wallet

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, state: StateReader, time: Time)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("leasing") {
    lease ~ cancel
  }

  @Path("/lease")
  @ApiOperation(value = "Creates a lease",
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
      dataType = "vsys.api.http.leasing.LeaseRequest"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Successful Operation")))
  def lease: Route = processRequest("lease", (t: LeaseRequest) => doBroadcast(TransactionFactory.lease(t, wallet, time)))

  @Path("/cancel")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Successful Operation")
  ))
  @ApiOperation(value = "Interrupt a lease",
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
      dataType = "vsys.api.http.leasing.LeaseCancelRequest"
    )
  ))
  def cancel: Route = processRequest("cancel", (t: LeaseCancelRequest) => doBroadcast(TransactionFactory.leaseCancel(t, wallet, time)))
}
