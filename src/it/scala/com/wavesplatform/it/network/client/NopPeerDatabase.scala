package com.wavesplatform.it.network.client

import java.net.{InetAddress, InetSocketAddress}

import vsys.network.PeerDatabase
import io.netty.channel.Channel

object NopPeerDatabase extends PeerDatabase {
  override def addCandidate(socketAddress: InetSocketAddress): Boolean = true
  override def touch(socketAddress: InetSocketAddress): Unit = {}
  override def blacklist(host: InetSocketAddress, reason: String): Unit = {}
  override def knownPeers: Map[InetSocketAddress, Long] = Map.empty
  override def blacklistedHosts: Set[InetAddress] = Set.empty
  override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None
  override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty
  override def clearBlacklist(): Unit = ()
  override def suspend(host: InetSocketAddress): Unit = {}
  override def suspendedHosts: Set[InetAddress] = Set.empty
  override def detailedSuspended: Map[InetAddress, Long] = Map.empty
  override def blacklistAndClose(channel: Channel, reason: String): Unit = channel.close()
  override def suspendAndClose(channel: Channel): Unit = channel.close()
}
