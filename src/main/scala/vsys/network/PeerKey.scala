package vsys.network

import java.net.InetAddress

case class PeerKey(host: InetAddress, nonce: Long)
