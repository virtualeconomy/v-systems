package vsys.network

import vsys.blockchain.state.ByteStr
import io.netty.channel.{ChannelDuplexHandler, ChannelHandlerContext, ChannelPromise}
import vsys.blockchain.block.Block
import vsys.utils.ScorexLogging

import scala.util.DynamicVariable

class OptimisticExtensionLoader extends ChannelDuplexHandler with ScorexLogging {

  private val hopefullyNextIds = new DynamicVariable(Seq.empty[ByteStr])
  private val nextExtensionBlocks = new DynamicVariable(Seq.empty[Block])
  private val discardNextBlocks = new DynamicVariable(false)

  private def loadNextPart(ctx: ChannelHandlerContext, blocks: Seq[Block]): Unit = if (blocks.size > 1) {
    // Receiving just one block usually means we've reached the end of blockchain. Pre-Netty nodes
    // didn't handle GetSignatures(lastBlockId) message properly, hence the check.
    log.trace(s"${id(ctx)} loading next part")
    hopefullyNextIds value_= blocks.view.map(_.uniqueId).reverseIterator.take(100).toSeq
    ctx.writeAndFlush(LoadBlockchainExtension(hopefullyNextIds.value))
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case ExtensionBlocks(extension) if discardNextBlocks.value =>
      discardNextBlocks value_= false
      log.debug(s"${id(ctx)} discarding just-loaded ${extension.length} blocks as requested")
    case ExtensionBlocks(extension) if extension.isEmpty =>
      log.debug(s"${id(ctx)} Blockchain is up to date")
      hopefullyNextIds value_= Seq.empty
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) if hopefullyNextIds.value.isEmpty =>
      loadNextPart(ctx, extension)
      log.trace(s"${id(ctx)} Passing extension with ${extension.length} blocks upstream")
      super.channelRead(ctx, msg)
    case ExtensionBlocks(extension) =>
      nextExtensionBlocks value_= extension
    case _ => super.channelRead(ctx, msg)
  }

  override def write(ctx: ChannelHandlerContext, msg: AnyRef, promise: ChannelPromise): Unit = msg match {
    case LoadBlockchainExtension(localIds) if hopefullyNextIds.value == localIds =>
      if (nextExtensionBlocks.value.isEmpty) {
        log.debug(s"${id(ctx)} Still waiting for extension to load")
        hopefullyNextIds value_= Seq.empty
      } else {
        log.debug(s"${id(ctx)} Extension already loaded")
        ctx.fireChannelRead(ExtensionBlocks(nextExtensionBlocks.value))
        loadNextPart(ctx, nextExtensionBlocks.value)
        nextExtensionBlocks value_= Seq.empty
      }
    case _: LoadBlockchainExtension if hopefullyNextIds.value.isEmpty =>
      super.write(ctx, msg, promise)
    case LoadBlockchainExtension(localIds) =>
      val notYetRequestedIds = hopefullyNextIds.value.dropWhile(_ != localIds.head)
      if (notYetRequestedIds.isEmpty || !hopefullyNextIds.value.containsSlice(notYetRequestedIds)) {
//        log.debug(s"${fmt("LOCAL IDS", localIds)}${fmt("HOPEFULLY NEXT", hopefullyNextIds)}${fmt("DIFF", notYetRequestedIds)}")
        discardNextBlocks value_= nextExtensionBlocks.value.isEmpty
        log.debug(s"${id(ctx)} Got unexpected known block ids${if (discardNextBlocks.value) ", will discard extension once ready" else ""}")
      }
      hopefullyNextIds value_= Seq.empty
      nextExtensionBlocks value_= Seq.empty
      super.write(ctx, msg, promise)
    case _ => super.write(ctx, msg, promise)
  }
}
