package vsys.api.http

import io.netty.channel.group.ChannelGroup
import vsys.blockchain.transaction.{Transaction, ValidationError}
import vsys.blockchain.UtxPool
import vsys.network._

import scala.concurrent.Future

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
}
