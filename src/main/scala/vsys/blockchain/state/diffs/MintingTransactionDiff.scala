package vsys.blockchain.state.diffs

import vsys.settings.FunctionalitySettings
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.MintingTransaction
import vsys.blockchain.consensus.SPoSCalc._

import scala.util.{Left, Right}

object MintingTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: MintingTransaction): Either[ValidationError, Diff] = {
    if (tx.currentBlockHeight != height)
      Left(GenericError(s"Invalid MintingTransaction, minting transaction height is different from the block height"))
    else if (tx.amount != MintingReward)
      Left(ValidationError.WrongMintingReward(tx.amount))
    else Right(Diff(
      height = height,
      tx = tx,
      portfolios = Map(tx.recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
    ))
  }
}

