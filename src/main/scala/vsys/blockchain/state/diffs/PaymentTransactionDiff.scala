package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.account.Account
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.{PaymentTransaction, ValidationError}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {
    for {
      proof <- tx.proofs.firstCurveProof
      sender = proof.publicKey
      repPortDiff: Map[Account, Portfolio] = Map(
        tx.recipient -> Portfolio(
          balance = tx.amount,
          LeaseInfo.empty,
          assets = Map.empty))
      senderPortDiff: Map[Account, Portfolio] = Map(
        sender.toAddress -> Portfolio(
          balance = -tx.amount - tx.transactionFee,
          LeaseInfo.empty,
          assets = Map.empty
        ))
      portDiff = repPortDiff combine senderPortDiff
    } yield Diff(
      height = height,
      tx = tx,
      portfolios = portDiff,
      chargedFee = tx.transactionFee
    )
  }
}