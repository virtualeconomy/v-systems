package vsys.it.network.client

import vsys.network.{HandshakeDecoder, HandshakeHandler, HandshakeTimeoutHandler, LegacyFrameCodec}
import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.duration._

class LegacyChannelInitializer(handshakeHandler: HandshakeHandler) extends ChannelInitializer[SocketChannel] {
  private val initialBytesToStrip = 4
  private val maxFieldLength      = 1024 * 1024
  private val lengthFieldLength   = 4
  override def initChannel(ch: SocketChannel): Unit =
  ch.pipeline()
    .addLast(
      new HandshakeDecoder(NopPeerDatabase),
      new HandshakeTimeoutHandler(30.seconds),
      handshakeHandler,
      new LengthFieldPrepender(lengthFieldLength),
      new LengthFieldBasedFrameDecoder(maxFieldLength, 0, lengthFieldLength, 0, initialBytesToStrip),
      new LegacyFrameCodec(NopPeerDatabase))
}
