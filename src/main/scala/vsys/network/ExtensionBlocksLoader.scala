package vsys.network

import vsys.blockchain.state.ByteStr
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import vsys.blockchain.block.Block
import vsys.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.DynamicVariable

class ExtensionBlocksLoader(
    blockSyncTimeout: FiniteDuration,
    peerDatabase: PeerDatabase) extends ChannelInboundHandlerAdapter with ScorexLogging {
  private val pendingSignatures = new DynamicVariable(Map.empty[ByteStr, Int])
  private val targetExtensionIds = new DynamicVariable(Option.empty[ExtensionIds])
  private val blockBuffer = new DynamicVariable(mutable.TreeMap.empty[Int, Block])
  private val currentTimeout = new DynamicVariable(Option.empty[ScheduledFuture[_]])

  private def cancelTimeout(): Unit = {
    currentTimeout.value.foreach(_.cancel(false))
    currentTimeout.value = None
  }

  override def channelInactive(ctx: ChannelHandlerContext): Unit = {
    cancelTimeout()
    super.channelInactive(ctx)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case xid@ExtensionIds(_, newIds) if pendingSignatures.value.isEmpty =>
      if (newIds.nonEmpty) {
        targetExtensionIds.value = Some(xid)
        pendingSignatures.value = newIds.zipWithIndex.toMap
        cancelTimeout()
        currentTimeout.value = Some(ctx.executor().schedule(blockSyncTimeout) {
          if (targetExtensionIds.value.contains(xid)) {
            peerDatabase.blacklistAndClose(ctx.channel(), "Timeout loading blocks")
          }
        })
        newIds.foreach(s => ctx.write(GetBlock(s)))
        ctx.flush()
      } else {
        log.debug(s"${id(ctx)} No new blocks to load")
        ctx.fireChannelRead(ExtensionBlocks(Seq.empty))
      }

    case b: Block if pendingSignatures.value.contains(b.uniqueId) =>
      blockBuffer.value += pendingSignatures.value(b.uniqueId) -> b
      pendingSignatures.value = pendingSignatures.value - b.uniqueId
      if (pendingSignatures.value.isEmpty) {
        cancelTimeout()
        log.trace(s"${id(ctx)} Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.value.values.toSeq

        for (tids <- targetExtensionIds.value) {
          if (tids.lastCommonId != newBlocks.head.reference) {
            peerDatabase.blacklistAndClose(ctx.channel(),s"Extension head reference ${newBlocks.head.reference} differs from last common block id ${tids.lastCommonId}")
          } else if (!newBlocks.sliding(2).forall {
              case Seq(b1, b2) => b1.uniqueId == b2.reference
              case _ => true
            }) {
            peerDatabase.blacklistAndClose(ctx.channel(),"Extension blocks are not contiguous, pre-check failed")
          } else {
            newBlocks.par.find(!_.signatureValid) match {
              case Some(invalidBlock) =>
                peerDatabase.blacklistAndClose(ctx.channel(),s"Got block ${invalidBlock.uniqueId} with invalid signature")
              case None =>
                log.trace(s"${id(ctx)} Chain is valid, pre-check passed")
                ctx.fireChannelRead(ExtensionBlocks(newBlocks))
            }
          }
        }

        targetExtensionIds.value = None
        blockBuffer.value.clear()
      }

    case _: ExtensionIds =>
      log.warn(s"${id(ctx)} Received unexpected extension ids while loading blocks, ignoring")
    case _ => super.channelRead(ctx, msg)
  }
}
