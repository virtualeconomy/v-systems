package vsys.network

import java.net.InetSocketAddress
import io.swagger.annotations._

case class PeerNetworkConnection(@ApiModelProperty(required = true, example = "127.0.0.1")
                                 host: String,
                                 @ApiModelProperty(required = true, example = "0")
                                 port: Int)

case class PeerInfo(
    remoteAddress: InetSocketAddress,
    declaredAddress: Option[InetSocketAddress],
    applicationName: String,
    applicationVersion: (Int, Int, Int),
    nodeName: String,
    nodeNonce: Long)
