package com.wavesplatform

import java.time.Duration
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

object Coordinator extends ScorexLogging {
  def processFork(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, stateReader: StateReader,
                  utxStorage: UtxPool, time: Time, settings: WavesSettings, miner: Miner, blockchainReadiness: AtomicBoolean)
                 (newBlocks: Seq[Block]): Either[ValidationError, BigInt] = {
    val extension = newBlocks.dropWhile(history.contains)

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>

        def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
          extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

        def forkApplicationResultEi: Either[ValidationError, BigInt] = extension.view
          .map(b => b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(b))
          .collectFirst { case (b, Left(e)) => b -> e }
          .fold[Either[ValidationError, BigInt]](Right(history.score())) {
          case (b, e) =>
            log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block ${b.uniqueId}: $e")
            Left(e)
        }

        for {
          commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
          _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
          droppedTransactions <- blockchainUpdater.removeAfter(lastCommonBlockId)
          score <- forkApplicationResultEi
        } yield {
          droppedTransactions.foreach(utxStorage.putIfNew)
          miner.lastBlockChanged()
          updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
          score
        }
      case None =>
        log.debug("No new blocks found in extension")
        Right(history.score())
    }
  }


  private def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: Duration): Boolean = {
    val expired = time.correctedTime() - history.lastBlock.get.timestamp < maxBlockchainAge.toNanos
    blockchainReadiness.compareAndSet(expired, !expired)
  }

  def processBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                   stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean, miner: Miner,
                   settings: WavesSettings)(newBlock: Block, local: Boolean): Either[ValidationError, BigInt] = {
    val newScore = for {
      _ <- appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(newBlock)
    } yield history.score()

    if (local || newScore.isRight) {
      updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
      miner.lastBlockChanged()
    }
    newScore
  }

  private def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                          stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings)
                         (block: Block): Either[ValidationError, Unit] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block ${block.uniqueId} at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, stateReader, settings, time.correctedTime())(block)
    _ <- blockchainUpdater.processBlock(block)
  } yield utxStorage.removeAll(block.transactionData.map(_.transaction))
  // TODO: change utxStorage to use ProcessedTransaction

  def processCheckpoint(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater)
                       (newCheckpoint: Checkpoint): Either[ValidationError, BigInt] =
    checkpoint.set(newCheckpoint).map { _ =>
      makeBlockchainCompliantWith(history, blockchainUpdater)(newCheckpoint)
      history.score()
    }


  private def makeBlockchainCompliantWith(history: History, blockchainUpdater: BlockchainUpdater)(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter {
      checkpoint => history.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = history.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.uniqueId}]")
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  val MaxTimeDrift: Long = Duration.ofSeconds(15).toNanos
  val MaxBlockTimeRange: Long = Duration.ofMillis(5000).toNanos

  private def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, currentTs: Long)
                                      (block: Block): Either[ValidationError, Unit] = {

    import PoSCalc._

    val fs = bcs.functionalitySettings
    val (sortStart, sortEnd) = (fs.requireSortedTransactionsAfter, Long.MaxValue)
    val blockTime = block.timestamp

    (for {
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), s"timestamp $blockTime is from future")
      // use same ordering
      /*
      _ <- Either.cond(blockTime < sortStart || blockTime > sortEnd ||
        block.transactionData.map(_.transaction).sorted(TransactionsOrdering.InUTXPool) == block.transactionData.map(_.transaction),
        (), "transactions are not sorted")
      */
      // if minting tx is the last tx in block, just drop it
      _ <- Either.cond(blockTime < sortStart || blockTime > sortEnd ||
        block.transactionData.map(_.transaction).dropRight(1).sorted(TransactionsOrdering.InUTXPool) == block.transactionData.map(_.transaction).dropRight(1),
        (), "transactions are not sorted")

      parent <- history.parent(block).toRight(s"history does not contain parent ${block.reference}")
      parentHeight <- history.heightOf(parent.uniqueId).toRight(s"history does not contain parent ${block.reference}")
      prevBlockData = parent.consensusData
      blockData = block.consensusData

      generator = block.signerData.generator
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature
      _ <- Either.cond(calcGs.sameElements(blockGs), (),
        s"declared generation signature ${blockGs.mkString} does not match calculated generation signature ${calcGs.mkString}")

      // the validation here need to be discussed
      effectiveBalance = state.effectiveBalance(generator)
      _ <- Either.cond(blockTime < fs.minimalGeneratingBalanceAfter || effectiveBalance >= MinimalEffectiveBalanceForGenerator, (),
        s"generator's effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")

      //TODO
      //check the generator's address for multi slots address case (VEE), checked by diff
      //check generator.address, compare mintTime and generator's slot id
      minterAddress = PublicKeyAccount.toAddress(generator)
      mintTime = block.consensusData.mintTime
      slotid = (mintTime / 1000000000L / Math.max(fs.mintingSpeed, 1L)) % fs.numOfSlots
      slotAddress = state.slotAddress(slotid.toInt)
      _ <- Either.cond(minterAddress.address == slotAddress.get, (), s"Minting address ${minterAddress.address} does not match the slot address ${slotAddress.get} of slot ${slotid}")
      //compare cntTime and mintTime
      _ <- Either.cond(Math.abs(currentTs-mintTime) < MaxBlockTimeRange, (), s"Block too old or from future, current time ${currentTs}, mint time ${mintTime}")


      _ <- Either.cond(block.transactionData.map(_.transaction).filter(_.transactionType == TransactionParser.TransactionType.MintingTransaction).size == 1,
           (), s"One and only one minting transaction allowed per block" )
    } yield ()).left.map(e => GenericError(s"Block ${block.uniqueId} is invalid: $e"))
  }

}
