package com.wavesplatform

import java.io.File
import java.security.Security
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.actor.RootActorSystem
import com.wavesplatform.history.{CheckpointServiceImpl, StorageFactory}
import com.wavesplatform.http.NodeApiRoute
import com.wavesplatform.matcher.Matcher
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{NetworkServer, PeerDatabaseImpl, PeerInfo, UPnP}
import com.wavesplatform.settings._
import com.wavesplatform.utils.forceStopApplication
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import scorex.account.AddressScheme
import scorex.api.http._
import vsys.api.http.spos.{SPOSApiRoute, SPOSBroadcastApiRoute}
import vsys.api.http.vsys._
import vsys.api.http.database.DbApiRoute
import scorex.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import scorex.block.Block
import vsys.consensus.spos.api.http.SposConsensusApiRoute
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time, TimeImpl}
import vsys.wallet.Wallet
import scorex.waves.http.DebugApiRoute
import vsys.db.openDB

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Try

class Application(val actorSystem: ActorSystem, val settings: VsysSettings) extends ScorexLogging {

  private val db = openDB(settings.dataDirectory)

  private val checkpointService = new CheckpointServiceImpl(db, settings.checkpointsSettings)
  private val (history, _, stateReader, blockchainUpdater) = StorageFactory(db, settings.blockchainSettings)
  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled

  private val wallet: Wallet = try {
    Wallet(settings.walletSettings)
  } catch {
    case e: IllegalStateException =>
      log.error(s"Failed to open wallet file '${settings.walletSettings.file.get.getAbsolutePath}")
      throw e
  }

  def run(): Unit = {
    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")

    checkGenesis()

    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val feeCalculator = new FeeCalculator(settings.feesSettings)
    val time: Time = new TimeImpl()

    val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)
    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)

    val utxStorage = new UtxPool(time, stateReader, history, feeCalculator, settings.blockchainSettings.functionalitySettings, settings.utxSettings)

    val blockchainReadiness = new AtomicBoolean(false)

    val miner = new Miner(allChannels, blockchainReadiness, blockchainUpdater, checkpointService,
      history, stateReader, settings, time, utxStorage, wallet)

    val network = new NetworkServer(checkpointService, blockchainUpdater, time, miner, stateReader, settings,
      history, utxStorage, peerDatabase, allChannels, establishedConnections, blockchainReadiness)

    miner.lastBlockChanged()

    val apiRoutes = Seq(
      BlocksApiRoute(settings.restAPISettings, settings.checkpointsSettings, history, allChannels, checkpointService, blockchainUpdater),
      TransactionsApiRoute(settings.restAPISettings, stateReader, history, utxStorage),
      SposConsensusApiRoute(settings.restAPISettings, stateReader, history, settings.blockchainSettings.functionalitySettings),
      WalletApiRoute(settings.restAPISettings, wallet),
      PaymentApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
      UtilsApiRoute(settings.restAPISettings),
      PeersApiRoute(settings.restAPISettings, network.connect, peerDatabase, establishedConnections),
      AddressApiRoute(settings.restAPISettings, wallet, stateReader, settings.blockchainSettings.functionalitySettings),
      DebugApiRoute(settings.restAPISettings, wallet, stateReader, history, peerDatabase, establishedConnections, blockchainUpdater, allChannels, utxStorage),
      //WavesApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
      //AssetsApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, stateReader, time),
      NodeApiRoute(settings.restAPISettings, () => this.shutdown()),
      //AssetsBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
      LeaseApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, stateReader, time),
      LeaseBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
      //AliasApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, stateReader),
      //AliasBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
      SPOSApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, stateReader),
      SPOSBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
      //ContractApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, stateReader),
      //ContractBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
      DbApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, stateReader)
    )

    val apiTypes = Seq(
      typeOf[BlocksApiRoute],
      typeOf[TransactionsApiRoute],
      typeOf[SposConsensusApiRoute],
      typeOf[WalletApiRoute],
      typeOf[PaymentApiRoute],
      typeOf[UtilsApiRoute],
      typeOf[PeersApiRoute],
      typeOf[AddressApiRoute],
      typeOf[DebugApiRoute],
      //typeOf[WavesApiRoute],
      //typeOf[AssetsApiRoute],
      typeOf[NodeApiRoute],
      //typeOf[AssetsBroadcastApiRoute],
      typeOf[LeaseApiRoute],
      typeOf[LeaseBroadcastApiRoute],
      //typeOf[AliasApiRoute],
      //typeOf[AliasBroadcastApiRoute],
      typeOf[SPOSApiRoute],
      typeOf[SPOSBroadcastApiRoute],
      //typeOf[ContractApiRoute],
      //typeOf[ContractBroadcastApiRoute],
      typeOf[DbApiRoute]
    )

    for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
      upnp.addPort(addr.getPort)
    }


    implicit val as = actorSystem
    implicit val materializer = ActorMaterializer()

    if (settings.restAPISettings.enable) {
      val combinedRoute: Route = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings.restAPISettings).compositeRoute
      val httpFuture = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 10.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    //on unexpected shutdown
    sys.addShutdownHook {
      network.shutdown()
      shutdown()
    }

    if (settings.matcherSettings.enable) {
      val matcher = new Matcher(actorSystem, wallet, utxStorage, allChannels, stateReader, history, settings.blockchainSettings, settings.restAPISettings, settings.matcherSettings)
      matcher.runMatcher()
    }
  }

  def checkGenesis(): Unit = if (history.isEmpty) {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(blockchainUpdater.processBlock)
      .left.foreach { value =>
        log.error(value.toString)
        forceStopApplication()
      }

    log.info("Genesis block has been added to the state")
  }

  @volatile var shutdownInProgress = false
  @volatile var serverBinding: ServerBinding = _

  def shutdown(): Unit = {
    if (!shutdownInProgress) {
      log.info("Stopping network services")
      shutdownInProgress = true
      if (settings.restAPISettings.enable) {
        Try(Await.ready(serverBinding.unbind(), 60.seconds)).failed.map(e => log.error("Failed to unbind REST API port: " + e.getMessage))
      }
      for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
        upnp.deletePort(addr.getPort)
      }

      Try(Await.result(actorSystem.terminate(), 60.seconds))
        .failed.map(e => log.error("Failed to terminate actor system: " + e.getMessage))
      log.info("Closing db")
      db.close()
      log.info("Shutdown complete")
    }
  }

}

object Application extends ScorexLogging {

  private def configureLogging(settings: VsysSettings) = {
    import ch.qos.logback.classic.{Level, LoggerContext}
    import org.slf4j._

    val lc = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = lc.getLogger(Logger.ROOT_LOGGER_NAME)
    settings.loggingLevel match {
      case LogLevel.TRACE => rootLogger.setLevel(Level.TRACE)
      case LogLevel.DEBUG => rootLogger.setLevel(Level.DEBUG)
      case LogLevel.INFO => rootLogger.setLevel(Level.INFO)
      case LogLevel.WARN => rootLogger.setLevel(Level.WARN)
      case LogLevel.ERROR => rootLogger.setLevel(Level.ERROR)
    }
  }

  private def readConfig(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("vsys")) {
          log.error("Malformed configuration file was provided! Aborting!")
          log.error("Please, read following article about configuration file format:")
          //log.error("https://github.com/wavesplatform/Waves/wiki/Waves-Node-configuration-file") // need to be replaced by vsys wiki
          forceStopApplication()
        }
        loadConfig(cfg)
    }

    config
  }

  def main(args: Array[String]): Unit = {
    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    System.setProperty("sun.net.inetaddr.ttl", "0")
    System.setProperty("sun.net.inetaddr.negative.ttl", "0")
    Security.setProperty("networkaddress.cache.ttl", "0")
    Security.setProperty("networkaddress.cache.negative.ttl", "0")

    log.info("Starting...")

    val config = readConfig(args.headOption)
    val settings = VsysSettings.fromConfig(config)
    Kamon.start(config)

    RootActorSystem.start("wavesplatform", config) { actorSystem =>
      configureLogging(settings)

      // Initialize global var with actual address scheme
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings) {
        override def shutdown(): Unit = {
          Kamon.shutdown()
          super.shutdown()
        }
      }.run()}
    }
  }
}
