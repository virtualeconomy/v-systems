package vsys.network

import java.util.concurrent.ScheduledFuture

import vsys.blockchain.state.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import vsys.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration
import scala.util.DynamicVariable

class ExtensionSignaturesLoader(syncTimeout: FiniteDuration, peerDatabase: PeerDatabase)
  extends ChannelDuplexHandler with ScorexLogging {

  private val currentTimeout = new DynamicVariable(Option.empty[ScheduledFuture[Unit]])
  private val lastKnownSignatures = new DynamicVariable(Seq.empty[ByteStr])

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case s: Signatures =>
      val (known, unknown) = s.signatures.span(id => lastKnownSignatures.value.contains(id))
      currentTimeout.value.foreach(_.cancel(true))
      currentTimeout value_= None
      known.lastOption.foreach { lastKnown =>
        log.debug(s"${id(ctx)} Got extension with ${known.length}/${s.signatures.length} known signatures")
        ctx.fireChannelRead(ExtensionIds(lastKnown, unknown))
      }
    case _ => super.channelRead(ctx, msg)
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    currentTimeout.value.foreach(_.cancel(false))
    currentTimeout value_= None
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LoadBlockchainExtension(sigs) if currentTimeout.value.isEmpty =>
      lastKnownSignatures value_= sigs

      log.debug(s"${id(ctx)} Loading extension, last ${sigs.length} are ${formatSignatures(sigs)}")

      currentTimeout value_= Some(ctx.executor().schedule(syncTimeout) {
        if (currentTimeout.value.nonEmpty && ctx.channel().isActive) {
          peerDatabase.blacklistAndClose(ctx.channel(),"Timeout expired while loading extension")
        }
      })

      ctx.writeAndFlush(GetSignatures(sigs), promise)

    case LoadBlockchainExtension(_) =>
      log.debug(s"${id(ctx)} Received request to load signatures while waiting for extension, ignoring for now")
      promise.setSuccess()

    case _ => super.write(ctx, msg, promise)
  }
}
