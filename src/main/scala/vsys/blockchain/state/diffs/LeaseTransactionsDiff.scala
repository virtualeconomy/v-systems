package vsys.blockchain.state.diffs

import cats._
import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.blockchain.state._
import vsys.blockchain.state.reader.StateReader
import vsys.account.Account
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof

import scala.util.{Left, Right}

object LeaseTransactionsDiff {

  def lease(s: StateReader, height: Int)(tx: LeaseTransaction): Either[ValidationError, Diff] = {
    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val recipient = tx.recipient
    if (recipient == sender.toAddress)
      Left(GenericError("Cannot lease to self"))
    else {
      val ap = s.accountPortfolio(sender)
      if (ap.balance - ap.leaseInfo.leaseOut < tx.amount) {
        Left(GenericError(s"Cannot lease more than own: Balance:${ap.balance}, already leased: ${ap.leaseInfo.leaseOut}"))
      }
      else {
        val portfolioDiff: Map[Account, Portfolio] = Map(
          sender.toAddress -> Portfolio(-tx.fee, LeaseInfo(0, tx.amount), Map.empty),
          recipient -> Portfolio(0, LeaseInfo(tx.amount, 0), Map.empty)
        )
        Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.id -> true), chargedFee = tx.fee))
      }
    }
  }

  def leaseCancel(s: StateReader, settings: FunctionalitySettings, time: Long, height: Int)
                 (tx: LeaseCancelTransaction): Either[ValidationError, Diff] = {
    val leaseEi = s.findTransaction[LeaseTransaction](tx.leaseId) match {
      case None => Left(GenericError(s"Related LeaseTransaction not found"))
      case Some(l) => Right(l)
    }
    for {
      lease <- leaseEi
      recipient = lease.recipient
      isLeaseActive = s.isLeaseActive(lease)
      leaseSender = EllipticCurve25519Proof.fromBytes(lease.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      _ <- if (!isLeaseActive)
        Left(GenericError(s"Cannot cancel already cancelled lease")) else Right(())
      canceller = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      preDiff: Map[Account, Portfolio] = Monoid.combine(
        Map(canceller.toAddress -> Portfolio(-tx.fee, LeaseInfo(0, -lease.amount), Map.empty)),
        Map(recipient -> Portfolio(0, LeaseInfo(-lease.amount, 0), Map.empty)))
      portfolioDiff <- if (canceller == leaseSender) {
        Right(preDiff)
      } else Left(GenericError(s"LeaseTransaction was leased by other sender"))

    } yield Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(lease.id -> false), chargedFee = tx.fee)
  }
}

