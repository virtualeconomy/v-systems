package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.ValidationError
import vsys.transaction.MintingTransaction
import vsys.spos.SPoSCalc._

import scala.util.{Left, Right}

object MintingTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: MintingTransaction): Either[ValidationError, Diff] = {

    if (tx.currentBlockHeight != height)
      Left(GenericError(s"Invalid MintingTransaction, minting transaction height is different from the block height"))
    else if (tx.amount != MintingReward)
      Left(ValidationError.WrongMintingReward(tx.amount))
    else
      Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.recipient -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty))))

  }
}

