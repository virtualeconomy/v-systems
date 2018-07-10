package com.wavesplatform.state2.diffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.patch.LeasePatch
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.transaction.{Signed, Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader,  pervBlockTimestamp : Option[Long])(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s, pervBlockTimestamp)(block.feesDistribution, block.timestamp, block.transactionData.map(_.transaction), 1) }

  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader, prevBlockTimestamp: Option[Long])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlockTimestamp)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff), prev)(block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block.timestamp))
    }._1

  private def apply(settings: FunctionalitySettings, s: StateReader, pervBlockTimestamp : Option[Long])(feesDistribution: Diff, timestamp: Long, txs: Seq[Transaction], heightDiff: Int) = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, pervBlockTimestamp, timestamp, currentBlockHeight) _

    val txsDiffEi = txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx)
        .map(newDiff => diff.combine(newDiff)))
    }

    txsDiffEi.map { d =>
      val diff = if (currentBlockHeight == settings.resetEffectiveBalancesAtHeight)
        Monoid.combine(d, LeasePatch(new CompositeStateReader(s, d.asBlockDiff)))
      else d
      val newSnapshots = diff.portfolios
        .collect { case (acc, portfolioDiff) if portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 =>
          val oldPortfolio = s.accountPortfolio(acc)
          val lastHeight = s.lastUpdateHeight(acc).getOrElse(0)
          val lastWeightedBalance = s.lastUpdateWeightedBalance(acc).getOrElse(0L)
          val newBalance = oldPortfolio.balance + portfolioDiff.balance
          val newEffectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance
          // this number should be confirmed later
          val maxUpdateBlocks = (24 * 60 * 60 / settings.mintingSpeed)*1L
          val newWeightedBalance = portfolioDiff.effectiveBalance == 0 && currentBlockHeight == lastHeight match {
            case true => lastWeightedBalance
            case _ => math.min(oldPortfolio.effectiveBalance/maxUpdateBlocks * math.min(maxUpdateBlocks, currentBlockHeight - lastHeight)
              + lastWeightedBalance/maxUpdateBlocks * (maxUpdateBlocks - math.min(maxUpdateBlocks, currentBlockHeight - lastHeight)),
              newEffectiveBalance)
          }
          acc -> SortedMap(currentBlockHeight -> Snapshot(
            prevHeight = lastHeight,
            balance = newBalance,
            effectiveBalance = newEffectiveBalance,
            weightedBalance = newWeightedBalance)
          )
        }
      BlockDiff(diff, heightDiff, newSnapshots)
    }
  }
}
