package vsys.blockchain.state.diffs

import cats.implicits._
import cats.Monoid
import vsys.blockchain.state._
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.state.reader.{CompositeStateReader, StateReader}
import vsys.blockchain.block.Block
import vsys.blockchain.transaction.{Signed, ValidationError}
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.blockchain.consensus.SPoSCalc
import vsys.settings.FunctionalitySettings
import vsys.utils.ScorexLogging

import scala.collection.SortedMap

object BlockDiffer extends ScorexLogging {

  def right(diff: Diff): Either[ValidationError, Diff] = Right(diff)

  def fromBlock(settings: FunctionalitySettings, s: StateReader, prevBlockTimestamp: Option[Long])(block: Block): Either[ValidationError, BlockDiff] =
    Signed.validateSignatures(block).flatMap { _ => apply(settings, s, prevBlockTimestamp)(block.feesDistribution, block.timestamp, block.transactionData, 1) }

  def unsafeDiffMany(settings: FunctionalitySettings, s: StateReader, prevBlockTimestamp: Option[Long])(blocks: Seq[Block]): BlockDiff =
    blocks.foldLeft((Monoid[BlockDiff].empty, prevBlockTimestamp)) { case ((diff, prev), block) =>
      val blockDiff = fromBlock(settings, new CompositeStateReader(s, diff), prev)(block).explicitGet()
      (Monoid[BlockDiff].combine(diff, blockDiff), Some(block.timestamp))
    }._1

  private def apply(settings: FunctionalitySettings,
                    s: StateReader,
                    prevBlockTimestamp: Option[Long])(feesDistribution: Diff,
                                                      timestamp: Long,
                                                      txs: Seq[ProcessedTransaction],
                                                      heightDiff: Int): Either[ValidationError, BlockDiff] = {
    val currentBlockHeight = s.height + 1

    val txDiffer = TransactionDiffer(settings, prevBlockTimestamp, timestamp, currentBlockHeight) _

    /**
      since we have some in block transactions with status not equal to success
      we need a much stricter validation about fee charge in later version
    */
    val txsDiffEi = txs.foldLeft(right(feesDistribution)) { case (ei, tx) => ei.flatMap(diff =>
      txDiffer(new CompositeStateReader(s, diff.asBlockDiff), tx.transaction) match {
        case Right(newDiff) => if (newDiff.chargedFee == tx.feeCharged && newDiff.txStatus == tx.status) {
          Right(diff.combine(newDiff))
        } else {
          println("tx status: " + tx.status)
          println("newDiff status: " + newDiff.txStatus)
          Left(TransactionValidationError(ValidationError.InvalidProcessedTransaction, tx.transaction))
        }
        case Left(l) => Left(l)
      })
    }

    txsDiffEi.map { d =>
      val newSnapshots = d.portfolios
        .collect { case (acc, portfolioDiff) if portfolioDiff.balance != 0 || portfolioDiff.effectiveBalance != 0 =>
          val oldPortfolio = s.accountPortfolio(acc)
          val lastHeight = s.lastUpdateHeight(acc).getOrElse(0)
          val lastWeightedBalance = s.lastUpdateWeightedBalance(acc).getOrElse(0L)
          val newBalance = oldPortfolio.balance + portfolioDiff.balance
          val newEffectiveBalance = oldPortfolio.effectiveBalance + portfolioDiff.effectiveBalance
          val newWeightedBalance = portfolioDiff.effectiveBalance == 0 && currentBlockHeight == lastHeight match {
            case true => lastWeightedBalance
            case _ => SPoSCalc.weightedBalaceCalc(currentBlockHeight - lastHeight, oldPortfolio.effectiveBalance,
              lastWeightedBalance, newEffectiveBalance, settings)
          }
          acc -> SortedMap(currentBlockHeight -> Snapshot(
            prevHeight = lastHeight,
            balance = newBalance,
            effectiveBalance = newEffectiveBalance,
            weightedBalance = newWeightedBalance)
          )
        }
      BlockDiff(d, heightDiff, newSnapshots)
    }
  }
}
