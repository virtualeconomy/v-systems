package vsys.network

import java.net.{InetSocketAddress, NetworkInterface}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean}

import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel._
import io.netty.channel.group.ChannelGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import vsys.Version
import vsys.blockchain.history.{History, CheckpointService}
import vsys.blockchain.mining.Miner
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.{BlockchainUpdater, UtxPool}
import vsys.settings._
import vsys.utils.{ScorexLogging, Time}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Random

class NetworkServer(checkpointService: CheckpointService,
                    blockchainUpdater: BlockchainUpdater,
                    time: Time,
                    miner: Miner,
                    stateReader: StateReader,
                    settings: VsysSettings,
                    history: History,
                    utxPool: UtxPool,
                    peerDatabase: PeerDatabase,
                    allChannels: ChannelGroup,
                    peerInfo: ConcurrentHashMap[Channel, PeerInfo],
                    blockchainReadiness: AtomicBoolean
                   ) extends ScorexLogging {

  private[this] val averageHandshakePeriod = 1.second
  private[this] val initialBytesToStrip    = 4
  private[this] val maxFrameLength         = 100 * 1024 * 1024
  private[this] val lengthFieldSize        = 4
  private[this] val initialCapacity        = 10
  private[this] val loadFactor             = 0.9f
  private[this] val concurrentLevel        = 10

  private var shutdownInitiated = false

  private val bossGroup = new NioEventLoopGroup()
  private val workerGroup = new NioEventLoopGroup()
  private val handshake =
    Handshake(Constants.ApplicationName + settings.blockchainSettings.addressSchemeCharacter, Version.VersionTuple,
      settings.networkSettings.nodeName, settings.networkSettings.nonce, settings.networkSettings.declaredAddress)

  private val scoreObserver = new RemoteScoreObserver(
    settings.synchronizationSettings.scoreTTL,
    history.lastBlockIds(settings.synchronizationSettings.maxRollback), history.score())

  private val discardingHandler = new DiscardingHandler(blockchainReadiness)
  private val messageCodec = new MessageCodec(peerDatabase)

  private val excludedAddresses: Set[InetSocketAddress] = {
    val bindAddress = settings.networkSettings.bindAddress
    val isLocal = Option(bindAddress.getAddress).exists(_.isAnyLocalAddress)
    val localAddresses = if (isLocal) {
      NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala
          .map(a => new InetSocketAddress(a, bindAddress.getPort)))
        .toSet
    } else Set(bindAddress)

    localAddresses ++ settings.networkSettings.declaredAddress.toSet
  }

  private val lengthFieldPrepender = new LengthFieldPrepender(lengthFieldSize)

  // There are two error handlers by design. WriteErrorHandler adds a future listener to make sure writes to network
  // succeed. It is added to the head of pipeline (it's the closest of the two to actual network), because some writes
  // are initiated from the middle of the pipeline (e.g. extension requests). FatalErrorHandler, on the other hand,
  // reacts to inbound exceptions (the ones thrown during channelRead). It is added to the tail of pipeline to handle
  // exceptions bubbling up from all the handlers below. When a fatal exception is caught (like OutOfMemory), the
  // application is terminated.
  private val writeErrorHandler = new WriteErrorHandler
  private val fatalErrorHandler = new FatalErrorHandler
  private val historyReplier = new HistoryReplier(history, settings.synchronizationSettings.maxChainLength)
  private val inboundConnectionFilter = new InboundConnectionFilter(peerDatabase,
    settings.networkSettings.maxInboundConnections,
    settings.networkSettings.maxConnectionsPerHost)

  private val coordinatorExecutor = new DefaultEventLoop

  private val coordinatorHandler = new CoordinatorHandler(checkpointService, history, blockchainUpdater, time,
    stateReader, utxPool, blockchainReadiness, miner, settings, peerDatabase, allChannels)

  val peerConnections = new ConcurrentHashMap[PeerKey, Channel](initialCapacity, loadFactor, concurrentLevel)

  private val serverHandshakeHandler =
    new HandshakeHandler.Server(handshake, peerInfo, peerConnections, peerDatabase, allChannels)

  private val utxPoolSynchronizer = new UtxPoolSynchronizer(utxPool, allChannels)

  def pipelineTail: Seq[ChannelHandlerAdapter] = Seq(
    lengthFieldPrepender,
    new LengthFieldBasedFrameDecoder(maxFrameLength, 0, lengthFieldSize, 0, initialBytesToStrip),
    new LegacyFrameCodec(peerDatabase),
    discardingHandler,
    messageCodec,
    new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval),
    historyReplier,
    new ExtensionSignaturesLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
    new ExtensionBlocksLoader(settings.synchronizationSettings.synchronizationTimeout, peerDatabase),
    new OptimisticExtensionLoader,
    utxPoolSynchronizer,
    scoreObserver,
    coordinatorHandler,
    fatalErrorHandler
  )

  private val serverChannel = settings.networkSettings.declaredAddress.map { _ =>
    new ServerBootstrap()
      .group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(
        new PipelineInitializer(
          Seq(
            inboundConnectionFilter,
            writeErrorHandler,
            new HandshakeDecoder(peerDatabase),
            new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
            serverHandshakeHandler
          ) ++ pipelineTail
      ))
      .bind(settings.networkSettings.bindAddress)
      .channel()
  }

  private val outgoingChannels = new ConcurrentHashMap[InetSocketAddress, Channel]

  private val clientHandshakeHandler =
    new HandshakeHandler.Client(handshake, peerInfo, peerConnections, peerDatabase)

  private val bootstrap = new Bootstrap()
    .option(ChannelOption.CONNECT_TIMEOUT_MILLIS, settings.networkSettings.connectionTimeout.toMillis.toInt: Integer)
    .group(workerGroup)
    .channel(classOf[NioSocketChannel])
    .handler(new PipelineInitializer(Seq(
      writeErrorHandler,
      new HandshakeDecoder(peerDatabase),
      new HandshakeTimeoutHandler(settings.networkSettings.handshakeTimeout),
      clientHandshakeHandler) ++ pipelineTail
    ))

  def scheduleConnectTask(): Unit = if (!shutdownInitiated) {
    val delay = (if (peerConnections.isEmpty) averageHandshakePeriod else 5.seconds) +
      (Random.nextInt(1000) - 500).millis // add some noise so that nodes don't attempt to connect to each other simultaneously
    log.trace(s"Next connection attempt in $delay")

    workerGroup.schedule(delay) {
      val outgoing = outgoingChannels.keySet.iterator().asScala.toVector

      def outgoingStr = outgoing.map(_.toString).sorted.mkString("[", ", ", "]")

      val all = peerInfo.values().iterator().asScala.flatMap(_.declaredAddress).toVector
      val incoming = all.filterNot(outgoing.contains)

      def incomingStr = incoming.map(_.toString).sorted.mkString("[", ", ", "]")

      log.trace(s"Outgoing: $outgoingStr ++ incoming: $incomingStr")
      if (outgoingChannels.size() < settings.networkSettings.maxOutboundConnections) {
        peerDatabase
          .randomPeer(excluded = excludedAddresses ++ all)
          .foreach(connect)
      }

      scheduleConnectTask()
    }
  }

  def formatOutgoingChannelEvent(channel: Channel, event: String): String = s"${id(channel)} $event, outgoing channel count: ${outgoingChannels.size()}"

  def handleOutgoingChannelClosed(remoteAddress: InetSocketAddress)(closeFuture: ChannelFuture): Unit = {
    outgoingChannels.remove(remoteAddress, closeFuture.channel())
    if (!shutdownInitiated) peerDatabase.suspendAndClose(closeFuture.channel())

    if (closeFuture.isSuccess)
      log.trace(formatOutgoingChannelEvent(closeFuture.channel(), "Channel closed (expected)"))
    else
      log.debug(
        formatOutgoingChannelEvent(
          closeFuture.channel(),
          s"Channel closed: ${Option(closeFuture.cause()).map(_.getMessage).getOrElse("no message")}"
        )
      )
  }

  def handleConnectionAttempt(remoteAddress: InetSocketAddress)(thisConnFuture: ChannelFuture): Unit = {
    if (thisConnFuture.isSuccess) {
      log.trace(formatOutgoingChannelEvent(thisConnFuture.channel(), "Connection established"))
      peerDatabase.touch(remoteAddress)
      thisConnFuture.channel().closeFuture().addListener(f => handleOutgoingChannelClosed(remoteAddress)(f))
    } else if (thisConnFuture.cause() != null) {
      peerDatabase.suspendAndClose(thisConnFuture.channel())
      outgoingChannels.remove(remoteAddress, thisConnFuture.channel())
      thisConnFuture.cause() match {
        case e: ClosedChannelException =>
          // this can happen when the node is shut down before connection attempt succeeds
          log.trace(
            formatOutgoingChannelEvent(
              thisConnFuture.channel(),
              s"Channel closed by connection issue: ${Option(e.getMessage).getOrElse("no message")}"
            )
          )
        case other => log.debug(formatOutgoingChannelEvent(thisConnFuture.channel(), other.getMessage))
      }
    }
  }

  def connect(remoteAddress: InetSocketAddress): Unit =
    outgoingChannels.computeIfAbsent(
      remoteAddress,
      _ => {
        val newConnFuture = bootstrap.connect(remoteAddress)

        log.trace(s"${id(newConnFuture.channel())} Connecting to $remoteAddress")
        newConnFuture.addListener(f => handleConnectionAttempt(remoteAddress)(f)).channel()
      }
    )

  scheduleConnectTask()

  def shutdown(): Unit = try {
    shutdownInitiated = true
    serverChannel.foreach(_.close().await())
    log.debug("Unbound server")
    allChannels.close().await()
    log.debug("Closed all channels")
  } finally {
    workerGroup.shutdownGracefully().await()
    bossGroup.shutdownGracefully().await()
    coordinatorExecutor.shutdownGracefully().await()
  }
}
