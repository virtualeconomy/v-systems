package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.account.Address
import scorex.transaction.{PaymentTransaction, ValidationError}

import scala.util.Right

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.recipient -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty)) combine Map(
          Address.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.amount - tx.fee,
            LeaseInfo.empty,
            assets = Map.empty
          ))
      ))
  }
}