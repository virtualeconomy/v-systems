package vsys.network

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.transaction.TransactionGen
import io.netty.buffer.Unpooled
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.util.DynamicVariable

class HandshakeDecoderSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "should read a handshake and remove itself from the pipeline" in {
    val mayBeDecodedHandshake: DynamicVariable[Option[Handshake]] = new DynamicVariable(None)

    val channel = new EmbeddedChannel(
      new HandshakeDecoder(PeerDatabase.NoOp),
      new ChannelInboundHandlerAdapter {
        override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = msg match {
          case x: Handshake => mayBeDecodedHandshake value_= Some(x)
          case _ =>
        }
      }
    )

    val origHandshake = new Handshake(
      applicationName = "vsysI",
      applicationVersion = (1, 2, 3),
      nodeName = "test",
      nodeNonce = 4,
      declaredAddress = None
    )

    val buff = Unpooled.buffer
    origHandshake.encode(buff)
    buff.writeCharSequence("foo", StandardCharsets.UTF_8)

    channel.writeInbound(buff)

    mayBeDecodedHandshake.value should contain(origHandshake)
  }

  private val invalidHandshakeBytes: Gen[Array[Byte]] = {
    // To bypass situations where the appNameLength > whole buffer and HandshakeDecoder waits for next bytes
    val appName = "x" * Byte.MaxValue
    val nodeName = "y" * Byte.MaxValue

    val appNameBytes = appName.getBytes(StandardCharsets.UTF_8)
    val versionBytes = Array(1, 2, 3).flatMap(Ints.toByteArray)
    val nodeNameBytes = nodeName.getBytes(StandardCharsets.UTF_8)
    val nonceBytes = Longs.toByteArray(1)
    val timestampBytes = Longs.toByteArray(System.currentTimeMillis() / 1000)

    val validDeclaredAddressLen = Set(0, 8, 20)
    val invalidBytesGen = Gen.listOfN(3, Arbitrary.arbByte.arbitrary).filter {
      case List(appNameLen, nodeNameLen, declaredAddressLen) =>
        !(appNameLen == appNameBytes.size || nodeNameLen == nodeNameBytes.size ||
          validDeclaredAddressLen.contains(declaredAddressLen))
      case _ =>
        false
    }

    invalidBytesGen.map {
      case List(appNameLen, nodeNameLen, declaredAddressLen) =>
        Array(appNameLen) ++
          appNameBytes ++
          versionBytes ++
          Array(nodeNameLen) ++
          nodeNameBytes ++
          nonceBytes ++
          Array(declaredAddressLen) ++
          timestampBytes
    }
  }

  "should blacklist a node sends an invalid handshake" in forAll(invalidHandshakeBytes) { bytes: Array[Byte] =>
    val decoder = new SpiedHandshakeDecoder
    val channel = new EmbeddedChannel(decoder)

    val buff = Unpooled.buffer
    buff.writeBytes(bytes)

    channel.writeInbound(buff)
    decoder.blockCalls.value shouldBe >(0)
  }

  private class SpiedHandshakeDecoder extends HandshakeDecoder(PeerDatabase.NoOp) {
    val blockCalls = new DynamicVariable(0)

    override protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
      blockCalls value_= blockCalls.value + 1
    }
  }

}