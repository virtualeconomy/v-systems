package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.account.Address
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{ByteStr, Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.Transaction
import vsys.blockchain.transaction.ValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation {

  def apply[T <: Transaction](s: StateReader, time: Long)(d: Diff): Either[AccountBalanceError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)

      val err = if (newPortfolio.balance < 0) {
        Some(s"negative vsys balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
      } else if (newPortfolio.assets.values.exists(_ < 0)) {
        Some(s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(newPortfolio)}")
      } else if (newPortfolio.effectiveBalance < 0) {
        Some(s"negative effective balance: $acc, old: ${leaseVSYSInfo(oldPortfolio)}, new: ${leaseVSYSInfo(newPortfolio)}")
      } else if (newPortfolio.balance < newPortfolio.leaseInfo.leaseOut) {
        Some(s"leased being more than own: $acc, old: ${leaseVSYSInfo(oldPortfolio)}, new: ${leaseVSYSInfo(newPortfolio)}")
      } else None
      err.map(acc -> _)
    }).toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def leaseVSYSInfo(p: Portfolio): (Long, LeaseInfo) = (p.balance, p.leaseInfo)

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] = p.assets.filter(_._2 < 0)
}
