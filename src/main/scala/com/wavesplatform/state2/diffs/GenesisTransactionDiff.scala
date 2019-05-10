package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{GenesisTransaction, ValidationError}

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(fs: FunctionalitySettings, height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else if (tx.slotId < fs.numOfSlots && tx.slotId >=0)
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.recipient -> Portfolio(
          balance = tx.amount,
          LeaseInfo.empty,
          assets = Map.empty)),
        slotids = Map(tx.slotId -> Option(tx.recipient.address)),
        addToSlot = Map(tx.recipient.address -> Option(tx.slotId)),
        slotNum = 1
      ))
    else
      Right(Diff(height = height, tx = tx,
        portfolios = Map(tx.recipient -> Portfolio(
          balance = tx.amount,
          LeaseInfo.empty,
          assets = Map.empty))))
  }
}
