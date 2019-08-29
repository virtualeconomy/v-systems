package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.{PaymentTransaction, ValidationError}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {
    for {
      proof <- EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr)
      sender = proof.publicKey
    } yield Diff(
      height = height,
      tx = tx,
      portfolios = Map(tx.recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
        combine Map(sender.toAddress -> Portfolio(-tx.amount - tx.fee, LeaseInfo.empty, Map.empty)),
      chargedFee = tx.fee
    )
  }
}