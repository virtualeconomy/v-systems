package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.account.Account
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.{PaymentTransaction, ValidationError}

import vsys.blockchain.transaction.proof.EllipticCurve25519Proof

import scala.util.Right

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {
    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val repPortDiff: Map[Account, Portfolio] = Map(
      tx.recipient -> Portfolio(
        balance = tx.amount,
        LeaseInfo.empty,
        assets = Map.empty))
    val senderPortDiff: Map[Account, Portfolio] = Map(
      sender.toAddress -> Portfolio(
        balance = -tx.amount - tx.fee,
        LeaseInfo.empty,
        assets = Map.empty
      ))
    val portDiff = repPortDiff combine senderPortDiff
    Right(Diff(height = height, tx = tx, portfolios = portDiff, chargedFee = tx.fee))
  }
}