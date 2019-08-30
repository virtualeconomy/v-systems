package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.settings.FunctionalitySettings
import vsys.blockchain.state._
import vsys.blockchain.state.reader.StateReader
import vsys.account.{Account, Address}
import vsys.blockchain.transaction.{AssetId, ValidationError}
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.assets.TransferTransaction

import scala.util.Right

object TransferTransactionDiff {
  def apply(state: StateReader, s: FunctionalitySettings, blockTime: Long, height: Int)(tx: TransferTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)

    val recipient = tx.recipient
    val portfolios = (
      tx.assetId match {
        case None => Map[Account, Portfolio](sender -> Portfolio(-tx.amount, LeaseInfo.empty, Map.empty)).combine(
          Map[Account, Portfolio](recipient -> Portfolio(tx.amount, LeaseInfo.empty, Map.empty))
        )
        case Some(aid) =>
          Map[Account, Portfolio](sender -> Portfolio(0, LeaseInfo.empty, Map(aid -> -tx.amount))).combine(
            Map[Account, Portfolio](recipient -> Portfolio(0, LeaseInfo.empty, Map(aid -> tx.amount)))
          )
      }).combine(
      tx.feeAssetId match {
        case None => Map[Account, Portfolio](sender -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty))
        case Some(aid) =>
          Map[Account, Portfolio](sender -> Portfolio(0, LeaseInfo.empty, Map(aid -> -tx.fee)))
      }
    )
    val assetIssued = check(tx.assetId, state)
    val feeAssetIssued = check(tx.feeAssetId, state)

    if (!(assetIssued && feeAssetIssued))
      Left(GenericError(s"Unissued assets are not allowed"))
    else
      Right(Diff(height = height,
        tx = tx,
        portfolios = portfolios,
        chargedFee = tx.fee))
  }

  private def check(a: Option[AssetId], state: StateReader): Boolean = {
    a match {
      case None => true
      case Some(aid) => state.assetInfo(aid).isDefined
    }
  }
}