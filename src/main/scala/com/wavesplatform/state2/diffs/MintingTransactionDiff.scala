package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{MintingTransaction, ValidationError}

import scala.util.{Left, Right}

object MintingTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: MintingTransaction): Either[ValidationError, Diff] = {

    if (tx.currentBlockHeight != height)
      Left(GenericError(s"Invalid MintingTransaction, minting transaction height is different from the block height"))
    else if (tx.amount != MintingTransaction.mintingReward || tx.fee < MintingTransaction.mintingFee)
      Left(GenericError(s"Minting Transaction Reward/Fee invalid"))
    else
      Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.sender.toAddress -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty))))

  }
}

