package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.{PaymentTransaction, ValidationError}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {
    for {
      proof <- tx.proofs.firstCurveProof
      sender = proof.publicKey
    } yield Diff(
      height = height,
      tx = tx,
      portfolios = Map(tx.recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
        combine Map(sender.toAddress -> Portfolio(-tx.amount - tx.transactionFee, LeaseInfo.empty, Map.empty)),
      chargedFee = tx.transactionFee
    )
  }
}