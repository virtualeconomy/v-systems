package com.wavesplatform.mining

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.network._
import com.wavesplatform.settings.VeeSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{Coordinator, UtxPool}
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import kamon.metric.instrument
import monix.eval.Task
import monix.execution._
import monix.execution.cancelables.{CompositeCancelable, SerialCancelable}
import scorex.account.{PrivateKeyAccount, Address}
import scorex.block.Block
import vee.consensus.spos.SposConsensusBlockData
import scorex.transaction.{BlockchainUpdater, CheckpointService, History, ProcessedTransaction, TransactionStatus}
import scorex.utils.{ScorexLogging, Time}
import vee.transaction.MintingTransaction
import vee.wallet.Wallet
import vee.spos.SPoSCalc._

import scala.concurrent.duration._
import scala.math.Ordering.Implicits._

class Miner(
               allChannels: ChannelGroup,
               blockchainReadiness: AtomicBoolean,
               blockchainUpdater: BlockchainUpdater,
               checkpoint: CheckpointService,
               history: History,
               stateReader: StateReader,
               settings: VeeSettings,
               timeService: Time,
               utx: UtxPool,
               wallet: Wallet) extends ScorexLogging {

  import Miner._

  private implicit val scheduler = Scheduler.fixedPool(name = "miner-pool", poolSize = 2)

  private val minerSettings = settings.minerSettings
  private val blockchainSettings = settings.blockchainSettings
  private lazy val processBlock = Coordinator.processBlock(checkpoint, history, blockchainUpdater, timeService, stateReader, utx, blockchainReadiness, Miner.this, settings) _

  private val scheduledAttempts = SerialCancelable()

  private val blockBuildTimeStats = Kamon.metrics.histogram("block-build-time", instrument.Time.Milliseconds)

  //Here use transfor to milliseconds
  //minerSettings.intervalAfterLastBlockThenGenerationIsAllowed is in milliseconds
  private def checkAge(parentHeight: Int, parent: Block): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), Duration.between(Instant.ofEpochMilli(parent.timestamp/1000000L), Instant.ofEpochMilli(timeService.correctedTime()/1000000L)))
      .left.flatMap(blockAge => Either.cond(blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed, (),
      s"BlockChain is too old (last block ${parent.uniqueId} generated $blockAge ago)"
    ))

  private def checkSlot(account:PrivateKeyAccount): Either[String,Int] =
    Either.cond(stateReader.addressToSlotID(account.address).isDefined,
      stateReader.addressToSlotID(account.address).get,
      s"Address ${account.address} is not in Slot List.")

  private def generateOneBlockTask(account: PrivateKeyAccount, parentHeight: Int, parent: Block,
                                   greatGrandParent: Option[Block], balance: Long, mintTime: Long)(delay: FiniteDuration): Task[Either[String, Block]] = Task {
    val pc = allChannels.size()
    lazy val lastBlockKernelData = parent.consensusData
    val currentTime = timeService.correctedTime()
    // start only use to record the duration
    val start = System.currentTimeMillis()
    log.debug(s"${start*1000000L}: Corrected time: $currentTime (in Nanoseonds)")
    for {
      _ <- Either.cond(pc >= minerSettings.quorum, (), s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
      _ = log.debug(s"Forging with ${account.address}, balance $balance, prev block ${parent.uniqueId}")
      _ <- checkSlot(account)
      _ = log.debug(s"Previous block ID ${parent.uniqueId} at $parentHeight with exact mint time ${lastBlockKernelData.mintTime}")
      avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
      consensusData = SposConsensusBlockData(mintTime, balance)
      _ <- Either.cond(minerSettings.rewardAddress == "" || Address.fromString(minerSettings.rewardAddress).isRight, (),
        s"Invalid reward address, can not send reward to ${minerSettings.rewardAddress}")
      rewardAddress: Address = minerSettings.rewardAddress match {
        case "" => account.toAddress
        case _ => Address.fromString(minerSettings.rewardAddress).right.get
      }
      unconfirmed = utx.packUnconfirmed() :+ ProcessedTransaction(TransactionStatus.Success, 0L, MintingTransaction.create(
        rewardAddress,  //minter can set any address here
        MintingTransaction.mintingReward,
        currentTime,
        parentHeight + 1
      ).right.get)
      _ = log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")
      block = Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account)
      // call currentTimeMillis to record the duration
      _ = blockBuildTimeStats.record(System.currentTimeMillis() - start)
    } yield block
  }.delayExecution(delay)

  private def generateBlockTask(account: PrivateKeyAccount): Task[Unit] = {
    val height = history.height()
    val lastBlock = history.lastBlock.get
    val totalSlots = settings.blockchainSettings.functionalitySettings.numOfSlots
    val mintingSpeed = Math.max(settings.blockchainSettings.functionalitySettings.mintingSpeed, 1L)
    val timeOfOneRound = totalSlots * mintingSpeed
    val nextConnectTime = 60000000000L.nanos
    val grandParent = history.parent(lastBlock, 2)
    (for {
      _ <- checkAge(height, lastBlock)
      // useful to deal with the delay cases (so keep here for detail design)
      // timeService unstable?
      ts = timeService.correctedTime()
    } yield ts) match {
      case Right(ts) =>
        val offset = checkSlot(account) match {
          case Right(id) => ((id * mintingSpeed * 1000000000L  + timeOfOneRound * 1000000000L - ts % (timeOfOneRound * 1000000000L)) % (timeOfOneRound * 1000000000L)).nanos
          case _ => nextConnectTime
        }
        log.debug(s"Current time $ts")
        log.debug(s"Next attempt for acc = $account in $offset")
        val balance = mintingBalance(stateReader, blockchainSettings.functionalitySettings, account, height)
        generateOneBlockTask(account, height, lastBlock, grandParent, balance, ts + offset.toNanos)(offset).flatMap {
          case Right(block) => Task.now {
            processBlock(block, true) match {
              case Left(err) => log.warn(err.toString)
              case Right(score) =>
                allChannels.broadcast(LocalScoreChanged(score))
                allChannels.broadcast(BlockForged(block))
            }
          }
          case Left(err) =>
            log.debug(s"No block generated because $err, retrying")
            generateBlockTask(account)
        }
      case Left(err) =>
        log.debug(s"Not scheduling block mining because $err")
        Task.unit
    }
  }

  def lastBlockChanged(): Unit = if (settings.minerSettings.enable) {
    log.debug("Miner notified of new block, restarting all mining tasks")
    scheduledAttempts := CompositeCancelable.fromSet(
      wallet.privateKeyAccounts.map(generateBlockTask).map(_.runAsync).toSet)
  } else {
    log.debug("Miner is disabled, ignoring last block change")
  }

  def shutdown(): Unit = ()
}

object Miner extends ScorexLogging {

  val Version: Byte = 1
  val MinimalGenerationOffsetMillis: Long = 1001

  //this function will not be called here
  def calcOffset(timeService: Time, calculatedTimestamp: Long): FiniteDuration = {
    // calculatedTimestamp in nanoseconds
    val calculatedGenerationTimestamp = (Math.ceil(calculatedTimestamp / 1000000000.0) * 1000000000L).toLong
    log.debug(s"CalculatedTS $calculatedTimestamp: CalculatedGenerationTS: $calculatedGenerationTimestamp")
    val calculatedOffset = calculatedGenerationTimestamp - timeService.correctedTime()
    // will return a duration, so millis is ok
    Math.max(MinimalGenerationOffsetMillis, calculatedOffset/1000000L).millis
  }
}
