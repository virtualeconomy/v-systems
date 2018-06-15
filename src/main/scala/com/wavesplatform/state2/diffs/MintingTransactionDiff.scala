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

    stateReader.paymentTransactionIdByHash(ByteStr(tx.hash)) match {
      case Some(existing) if blockTime >= settings.requirePaymentUniqueIdAfter => Left(GenericError(s"PaymentTx is already registered: $existing"))
      case _ => Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.minter.toAddress-> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty)),
      paymentTransactionIdsByHashes = Map(ByteStr(tx.hash) -> tx.id)
      ))
    }
  }
}
