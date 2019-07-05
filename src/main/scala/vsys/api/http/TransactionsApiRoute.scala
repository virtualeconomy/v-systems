package vsys.api.http

import javax.ws.rs.Path

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import vsys.account.Address
import vsys.blockchain.history.History
import vsys.blockchain.state.ByteStr
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.{Transaction, ProcessedTransaction}
import vsys.blockchain.UtxPool
import vsys.settings.{RestAPISettings, StateSettings}

import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Api(value = "/transactions", description = "Information about transactions")
case class TransactionsApiRoute(
    settings: RestAPISettings,
    stateSettings: StateSettings,
    state: StateReader,
    history: History,
    utxPool: UtxPool) extends ApiRoute with CommonApiFunctions {

  import TransactionsApiRoute.{MaxTransactionsPerRequest, MaxTransactionOffset}

  override lazy val route =
    pathPrefix("transactions") {
      customRoute
    }

  var customRoute = unconfirmed ~ addressLimit ~ info ~ activeLeaseList

  if(settings.customApiSettings.transactionsApiSettings.addressTransactionCount) {
    customRoute = customRoute ~ transactionCount
  }

  if(settings.customApiSettings.transactionsApiSettings.addressTransactionList) {
    customRoute = customRoute ~ transactionList
  }

  @Path("/count")
  @ApiOperation(value = "Count",
                notes = "Get count of transactions where specified address has been involved. *This is a custom api, you need to enable it in configuration file.*",
                httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "wallet address ", required = true, dataType = "string", paramType = "query"),
    new ApiImplicitParam(name = "txType", value = "transaction type", required = false, dataType = "integer", paramType = "query")
  ))
  def transactionCount: Route = (path("count") & get) {
    parameters('address, 'txType.?) { (addressStr, txTypeStrOpt) =>
      Address.fromString(addressStr) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          txTypeStrOpt match {
            case None =>
              complete(Json.obj("count" -> state.accountTransactionsLengths(a)))
            case Some(txTypeStr) =>
              Exception.allCatch.opt(TransactionType(txTypeStr.toInt)) match {
                case Some(txType: TransactionType.Value) =>
                  if(stateSettings.txTypeAccountTxIds){
                    complete(Json.obj("count" -> state.txTypeAccTxLengths(txType, a)))
                  }
                  else {
                    complete(unsupportedStateError("tx-type-account-tx-ids"))
                  }
                case _ => complete(invalidTxType)
              }
          }
      }
    }
  }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")

  private val invalidOffset = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.offset")

  private val invalidTxType = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.txType")

  @Path("/list")
  @ApiOperation(value = "List",
                notes = "Get list of transactions where specified address has been involved. *This is a custom api, you need to enable it in configuration file.*",
                httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "wallet address ", required = true, dataType = "string", paramType = "query"),
    new ApiImplicitParam(name = "txType", value = "transaction type ", required = false, dataType = "integer", paramType = "query"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "integer", paramType = "query"),
    new ApiImplicitParam(name = "offset", value = "Specified number of records offset", required = false, dataType = "integer", paramType = "query")
  ))
  def transactionList: Route = (path("list") & get) {
    parameters('address, 'txType.?, 'limit, 'offset ? "0") { (addressStr, txTypeStrOpt, limitStr, offsetStr) =>
      Address.fromString(addressStr) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          Exception.allCatch.opt(limitStr.toInt) match {
            case Some(limit) if limit > 0 && limit <= MaxTransactionsPerRequest =>
              Exception.allCatch.opt(offsetStr.toInt) match {
                case Some(offset) if offset >= 0 && offset <= MaxTransactionOffset =>
                  txTypeStrOpt match {
                    case None =>
                      complete(txListWrapper(state.accountTransactions(a, limit, offset)))
                    case Some(txTypeStr) =>
                      Exception.allCatch.opt(TransactionType(txTypeStr.toInt)) match {
                        case Some(txType: TransactionType.Value) =>
                          if(stateSettings.txTypeAccountTxIds){
                            complete(txListWrapper(state.txTypeAccountTransactions(txType, a, limit, offset)))
                          }
                          else {
                            complete(unsupportedStateError("tx-type-account-tx-ids"))
                          }
                        case _ => complete(invalidTxType)
                      }
                  }
                case _ =>
                  complete(invalidOffset)
              }
            case Some(limit) if limit > MaxTransactionsPerRequest =>
              complete(TooBigArrayAllocation)
            case _ =>
              complete(invalidLimit)
          }
      }
    }
  }

  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "limit", value = "Specified number of records to be returned", required = true, dataType = "integer", paramType = "path")
  ))
  //remove after all related productions being updated
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
                    complete(Json.arr(JsArray(state.accountTransactions(a, limit, 0)._2.map { case (h, tx) =>
                      processedTxToExtendedJson(tx) + ("height" -> JsNumber(h))
                    })))
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
      case lease: LeaseTransaction =>
        tx.json ++ Json.obj("leaseStatus" -> (if (state.isLeaseActive(lease)) "active" else "canceled"))
      case _ => tx.json
    }
  }

  private def txListWrapper(resFromReader: (Int, Seq[(Int, _ <: ProcessedTransaction)])): JsObject = {
    Json.obj(
      "totalCount" -> resFromReader._1,
      "size" -> resFromReader._2.size,
      "transactions" -> resFromReader._2.map { case (h, tx) =>
        processedTxToExtendedJson(tx) + ("height" -> JsNumber(h))
      }
    )
  }

  private def unsupportedStateError(stateName: String) = {
    StatusCodes.BadRequest -> Json.obj("message" -> s"State $stateName is not enabled at this node. You can turn on the state by editing configuration file.")
  }
}

object TransactionsApiRoute {
  val MaxTransactionsPerRequest = 10000
  val MaxTransactionOffset = 10000000
}
