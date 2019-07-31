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
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.leasing.LeaseRequest",
      defaultValue = "{\n\t\"amount\": 100000000,\n\t\"recipient\": \"3NBsppTVpai9jq6agi9wXXrWhaMPPig48Aw\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000,\n\t\"feeScale\": 100\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def lease: Route = processRequest("lease", (t: LeaseRequest) => doBroadcast(TransactionFactory.lease(t, wallet, time)))

  @Path("/cancel")
  @ApiOperation(value = "Interrupt a lease",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "vsys.api.http.leasing.LeaseCancelRequest",
      defaultValue = "{\n\t\"sender\": \"3Myss6gmMckKYtka3cKCM563TBJofnxvfD7\",\n\t\"txId\": \"ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV\",\n\t\"fee\": 10000000,\n\t\"feeScale\": 100\n}"
    )
  ))
  def cancel: Route = processRequest("cancel", (t: LeaseCancelRequest) => doBroadcast(TransactionFactory.leaseCancel(t, wallet, time)))
}
