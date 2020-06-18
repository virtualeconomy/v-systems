package vsys.api.http

import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler
import vsys.blockchain.transaction.{Transaction, ValidationError}
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.UtxPool
import vsys.network._
import vsys.utils.Schedulers._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait BroadcastRoute {
  def utx: UtxPool

  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    (for {
      tx <- v
      utxResult <- utx.putIfNew(tx)
    } yield {
      if (utxResult) {
        allChannels.broadcastTx(tx, None)
      }
      tx
    }).left.map(ApiError.fromValidationError)

  }

  protected def doBroadcastWithLimit(v: Either[ValidationError, Transaction], limitedScheduler: Scheduler): Future[Either[ApiError, Transaction]] = Future {
    (for {
      tx <- v
      g = executeCatchingInterruptedException(limitedScheduler)(utx.putIfNew(tx)).recover {
        case _ =>
          Left(GenericError("Wait to much time in validation"))
      }
      utxResult <- Await.result(g, Duration.Inf)
    } yield {
      if (utxResult) {
        allChannels.broadcastTx(tx, None)
      }
      tx
    }).left.map(ApiError.fromValidationError)

  }
}
