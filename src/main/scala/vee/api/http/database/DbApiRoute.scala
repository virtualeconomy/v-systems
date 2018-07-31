package vee.api.http.database

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http._
import vee.database.Entry
import scorex.transaction._
import vee.transaction.database.DbPutTransaction
import scorex.utils.Time
import vee.wallet.Wallet

@Path("/database")
@Api(value = "/database")
case class DbApiRoute (settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: StateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("database") {
    putKV ~ getKV
  }

  @Path("/put")
  @ApiOperation(value = "create/modify a db entry",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.database.DbPutRequest",
      defaultValue = "{\n\t\"name\": \"name\",\n\t\"data\": \"dbdata\",\n\t\"dataType\": \"ByteArray\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def putKV: Route = processRequest("put", (t: DbPutRequest) => doBroadcast(TransactionFactory.dbPut(t, wallet, time)))

  @Path("/get/{nameSpace}/{name}")
  @ApiOperation(value = "get", notes = "Get db entry", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "nameSpace", value = "Address", required = true, dataType = "String", paramType = "path"),
    new ApiImplicitParam(name = "name", value = "name", required = true, dataType = "String", paramType = "path")
  ))
  def getKV: Route = {
    (path("get" / Segment / Segment) & get) { case (addr, name) => {
      val dbEntryByteToResult = (dbEntryBytes: ByteStr) =>
        Entry.fromBytes(dbEntryBytes.arr).left.map[ApiError](ve1 => invalidDbEntry).map(entry=>entry.json)
      val addrKeyToResult = (addr1: Address) =>
        state.dbGet(DbPutTransaction.generateKey(addr1, name))
        .toRight(dbEntryNotExist(name, addr))
        .flatMap(dbEntryByteToResult)

      complete(Address.fromString(addr).left.map[ApiError](ve => invalidDbNameSpace(addr))
        .flatMap(addrKeyToResult))
      }
    }
  }
}
