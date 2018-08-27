package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Portfolio, _}
import scorex.account.Address
import scorex.transaction.ValidationError.{GenericError, Mistiming}
import scorex.transaction._
import scorex.transaction.assets._
import vee.transaction.proof.EllipticCurve25519Proof

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours

  def disallowSendingGreaterThanBalance[T <: Transaction](s: StateReader, settings: FunctionalitySettings, blockTime: Long, tx: T): Either[ValidationError, T] =

    tx match {
      case ptx: PaymentTransaction if s.accountPortfolio(EllipticCurve25519Proof.fromBytes(ptx.proofs.proofs.head.bytes.arr).toOption.get.publicKey).balance < (ptx.amount + ptx.fee) =>
        Left(GenericError(s"Attempt to pay unavailable funds: balance " +
          s"${s.accountPortfolio(EllipticCurve25519Proof.fromBytes(ptx.proofs.proofs.head.bytes.arr).toOption.get.publicKey).balance} is less than ${ptx.amount + ptx.fee}"))
      case ttx: TransferTransaction =>
        val sender: Address = ttx.sender

        val amountDiff = ttx.assetId match {
          case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -ttx.amount))
          case None => Portfolio(-ttx.amount, LeaseInfo.empty, Map.empty)
        }
        val feeDiff = ttx.feeAssetId match {
          case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -ttx.fee))
          case None => Portfolio(-ttx.fee, LeaseInfo.empty, Map.empty)
        }

        val accountPortfolio = s.accountPortfolio(sender)
        val spendings = Monoid.combine(amountDiff, feeDiff)

        lazy val negativeAsset = spendings.assets.find { case (id, amt) => (accountPortfolio.assets.getOrElse(id, 0L) + amt) < 0L }.map { case (id, amt) => (id, accountPortfolio.assets.getOrElse(id, 0L), amt, accountPortfolio.assets.getOrElse(id, 0L) + amt) }
        lazy val newVEEBalance = accountPortfolio.balance + spendings.balance
        lazy val negativeVEE = newVEEBalance < 0
        if (negativeVEE)
          Left(GenericError(s"Attempt to transfer unavailable funds:" +
            s" Transaction application leads to negative vee balance to (at least) temporary negative state, current balance equals ${accountPortfolio.balance}, spends equals ${spendings.balance}, result is $newVEEBalance"))
        else if (negativeAsset.nonEmpty)
          Left(GenericError(s"Attempt to transfer unavailable funds:" +
            s" Transaction application leads to negative asset '${negativeAsset.get._1}' balance to (at least) temporary negative state, current balance is ${negativeAsset.get._2}, spends equals ${negativeAsset.get._3}, result is ${negativeAsset.get._4}"))
        else Right(tx)
      case _ => Right(tx)
    }

  def disallowDuplicateIds[T <: Transaction](state: StateReader, settings: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = {
    if (state.containsTransaction(tx.id))
        Left(GenericError(s"Tx id cannot be duplicated. Current height is: $height. Tx with such id already present"))
    else Right(tx)
  }

  def disallowTxFromFuture[T <: Transaction](time: Long, tx: T): Either[ValidationError, T] = {

    if ((tx.timestamp - time) > MaxTimeTransactionOverBlockDiff.toNanos)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toNanos =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
      case _ => Right(tx)
    }

  def disallowInvalidFeeScale[T <: Transaction](tx: T) : Either[ValidationError, T] = {
    if (tx.assetFee._3 != 100){
      Left(ValidationError.WrongFeeScale(tx.assetFee._3))
    } else{
      Right(tx)
    }
  }
}


