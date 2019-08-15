package vsys.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.collect.EvictingQueue
import vsys.settings.NetworkSettings
import vsys.utils.createMVStore
import org.h2.mvstore.MVMap
import vsys.utils.{LogMVMapBuilder, ScorexLogging}

import io.netty.channel.Channel
import io.netty.channel.socket.nio.NioSocketChannel

import scala.collection.JavaConverters._
import scala.util.Random

class PeerDatabaseImpl(settings: NetworkSettings) extends PeerDatabase with AutoCloseable with ScorexLogging {

  private val database = createMVStore(settings.file)
  private val peersPersistence = database.openMap("peers", new LogMVMapBuilder[InetSocketAddress, Long])
  private val blacklist = database.openMap("blacklist", new LogMVMapBuilder[InetAddress, Long])
  private val suspension = database.openMap("suspension", new LogMVMapBuilder[InetAddress, Long])
  private val reasons = database.openMap("reasons", new LogMVMapBuilder[InetAddress, String])
  private val unverifiedPeers = EvictingQueue.create[InetSocketAddress](settings.maxUnverifiedPeers)

  for (a <- settings.knownPeers.view.map(inetSocketAddress(_, 9923))) {
    // add peers from config with max timestamp so they never get evicted from the list of known peers
    doTouch(a, Long.MaxValue)
  }

  override def addCandidate(socketAddress: InetSocketAddress): Boolean = {
    val r = !socketAddress.getAddress.isAnyLocalAddress &&
      !(socketAddress.getAddress.isLoopbackAddress && socketAddress.getPort == settings.bindAddress.getPort) &&
      Option(peersPersistence.get(socketAddress)).isEmpty &&
      !unverifiedPeers.contains(socketAddress)
    if (r) unverifiedPeers.add(socketAddress)
    r
  }

  private def doTouch(socketAddress: InetSocketAddress, timestamp: Long): Unit = unverifiedPeers.synchronized {
    unverifiedPeers.removeIf(_ == socketAddress)
    peersPersistence.compute(socketAddress, (_, prevTs) => Option(prevTs).fold(timestamp)(_.max(timestamp)))
    database.commit()
  }

  override def touch(socketAddress: InetSocketAddress): Unit = doTouch(socketAddress, System.currentTimeMillis())

  override def blacklist(socketAddress: InetSocketAddress, reason: String): Unit = getAddress(socketAddress).foreach { address =>
    unverifiedPeers.synchronized {
      unverifiedPeers.removeIf { x =>
        Option(x.getAddress).contains(address)
      }
      blacklist.put(address, System.currentTimeMillis())
      reasons.put(address, reason)
    }
  }

  override def suspend(socketAddress: InetSocketAddress): Unit = getAddress(socketAddress).foreach { address =>
    unverifiedPeers.synchronized {
      unverifiedPeers.removeIf { x =>
        Option(x.getAddress).contains(address)
      }
      suspension.put(address, System.currentTimeMillis())
    }
  }

  override def knownPeers: Map[InetSocketAddress, Long] = {
    removeObsoleteRecords(peersPersistence, settings.peersDataResidenceTime.toMillis)
      .asScala.toMap.filterKeys(address => !blacklistedHosts.contains(address.getAddress))
  }

  override def blacklistedHosts: Set[InetAddress] =
    removeObsoleteRecords(blacklist, settings.blackListResidenceTime.toMillis).keySet().asScala.toSet

  override def suspendedHosts: Set[InetAddress] =
    removeObsoleteRecords(suspension, settings.suspensionResidenceTime.toMillis).keySet().asScala.toSet

  override def detailedBlacklist: Map[InetAddress, (Long, String)] =
    removeObsoleteRecords(suspension, settings.blackListResidenceTime.toMillis).asScala.map{
      case ((h, t)) => h -> ((t, Option(reasons.get(h)).getOrElse("")))
    }.toMap

  override def detailedSuspended: Map[InetAddress, Long] =
    removeObsoleteRecords(suspension, settings.suspensionResidenceTime.toMillis).asScala.toMap

  override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = unverifiedPeers.synchronized {
    log.trace(s"Excluding: $excluded")
    def excludeAddress(isa: InetSocketAddress): Boolean = {
      excluded(isa) || Option(isa.getAddress).exists(blacklistedHosts) || suspendedHosts(isa.getAddress)
    }

    // excluded only contains local addresses, our declared address, and external declared addresses we already have
    // connection to, so it's safe to filter out all matching candidates
    unverifiedPeers.removeIf(isa => excluded(isa))
    log.trace(s"Evicting queue: $unverifiedPeers")
    val unverified = Option(unverifiedPeers.peek()).filterNot(excludeAddress)
    val verified = Random.shuffle(knownPeers.keySet.diff(excluded).toSeq).headOption.filterNot(excludeAddress)

    log.trace(s"Unverified: $unverified; Verified: $verified")
    (unverified, verified) match {
      case (Some(_), v@Some(_)) => if (Random.nextBoolean()) Some(unverifiedPeers.poll()) else v
      case (Some(_), None) => Some(unverifiedPeers.poll())
      case (None, v@Some(_)) => v
      case _ => None
    }
  }

  private def removeObsoleteRecords[T](map: MVMap[T, Long], maxAge: Long) = {
    val earliestValidTs = System.currentTimeMillis() - maxAge

    map.entrySet().asScala.collect {
      case e if e.getValue < earliestValidTs => e.getKey
    }.foreach(map.remove)

    database.commit()

    map
  }

  def clearBlacklist(): Unit = {
    blacklist.clear()
    reasons.clear()
  }

  override def close(): Unit = database.close()

  override def blacklistAndClose(channel: Channel, reason: String): Unit = getRemoteAddress(channel).foreach { x =>
    log.debug(s"Blacklisting ${id(channel)}: $reason")
    blacklist(x, reason)
    channel.close()
  }

  override def suspendAndClose(channel: Channel): Unit = getRemoteAddress(channel).foreach { x =>
    log.debug(s"Suspending ${id(channel)}")
    suspend(x)
    channel.close()
  }

  private def getAddress(socketAddress: InetSocketAddress): Option[InetAddress] = {
    val r = Option(socketAddress.getAddress)
    if (r.isEmpty) log.debug(s"Can't obtain an address from $socketAddress")
    r
  }

  private def getRemoteAddress(channel: Channel): Option[InetSocketAddress] = channel match {
    case x: NioSocketChannel => Option(x.remoteAddress())
    case x =>
      log.debug(s"Doesn't know how to get a remoteAddress from $x")
      None
  }

}
