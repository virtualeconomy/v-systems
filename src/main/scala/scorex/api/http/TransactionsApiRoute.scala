package scorex.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{History, Transaction}
import vsys.transaction.ProcessedTransaction

import vsys.transaction.proof.EllipticCurve25519Proof

import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(
    settings: RestAPISettings,
    state: StateReader,
    history: History,
    utxPool: UtxPool) extends ApiRoute with CommonApiFunctions {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ activeLeaseList
    }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "integer", paramType = "path")
  ))
  def addressLimit: Route = (pathPrefix("address") & get) {
    pathPrefix(Segment) { address =>
      Address.fromString(address) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          pathPrefix("limit") {
            pathEndOrSingleSlash {
              complete(invalidLimit)
            } ~
              path(Segment) { limitStr =>
                Exception.allCatch.opt(limitStr.toInt) match {
                  case Some(limit) if limit > 0 && limit <= MaxTransactionsPerRequest =>
                    complete(Json.arr(JsArray(state.accountTransactions(a, limit).map{ case (h, tx) =>
                      processedTxToExtendedJson(tx) + ("height" -> JsNumber(h))
                    }
                    )))
                  case Some(limit) if limit > MaxTransactionsPerRequest =>
                    complete(TooBigArrayAllocation)
                  case _ =>
                    complete(invalidLimit)
                }
              }
          } ~ complete(StatusCodes.NotFound)
      }
    }
  }

  @Path("/activeLeaseList/{address}")
  @ApiOperation(value = "Address", notes = "Get list of active lease transactions", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
  ))
  def activeLeaseList: Route = (pathPrefix("activeLeaseList") & get) {
    pathPrefix(Segment) { address =>
      Address.fromString(address) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          complete(Json.arr(JsArray(state.activeLeases().flatMap(state.transactionInfo)
              .map(a => (a._1,a._2,a._2.transaction))
              .collect{
                case (h:Int, tx:ProcessedTransaction, lt:LeaseTransaction)
                  if EllipticCurve25519Proof.fromBytes(lt.proofs.proofs.head.bytes.arr).toOption.get.publicKey.address == address
                  || state.resolveAliasEi(lt.recipient).toOption.get.address == address =>
                  processedTxToExtendedJson(tx) + ("height" -> JsNumber(h))
              }
          )))
      }
    }
  }

  @Path("/info/{id}")
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "transaction id ", required = true, dataType = "string", paramType = "path")
  ))
  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            state.transactionInfo(id) match {
              case Some((h, tx)) =>
                complete(processedTxToExtendedJson(tx) + ("height" -> JsNumber(h)))
              case None =>
                complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(utxPool.all().map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @ApiOperation(value = "Size of UTX pool", notes = "Get number of unconfirmed transactions in the UTX pool", httpMethod = "GET")
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utxPool.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @ApiOperation(value = "Transaction Info", notes = "Get transaction that is in the UTX", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Transaction id ", required = true, dataType = "string", paramType = "path")
  ))
  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            utxPool.transactionById(id) match {
              case Some(tx) =>
                complete(txToExtendedJson(tx))
              case None =>
                complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in UTX"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    tx match {
      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json ++ Json.obj("lease" -> state.findTransaction[LeaseTransaction](leaseCancel.leaseId).map(_.json).getOrElse[JsValue](JsNull))
      case t => t.json
    }
  }

  private def processedTxToExtendedJson(tx: ProcessedTransaction): JsObject = {
    tx.transaction match {
      case leaseCancel: LeaseCancelTransaction =>
        tx.json ++ Json.obj("lease" -> state.findTransaction[LeaseTransaction](leaseCancel.leaseId).map(_.json).getOrElse[JsValue](JsNull))
      case _ => tx.json
    }
  }
}

object TransactionsApiRoute {
  val MaxTransactionsPerRequest = 10000
}
