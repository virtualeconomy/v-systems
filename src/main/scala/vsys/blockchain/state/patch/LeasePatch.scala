package vsys.blockchain.state.patch

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.transaction.TransactionStatus

object LeasePatch {
  def apply(s: StateReader): Diff = {

    def invertLeaseInfo(l: LeaseInfo): LeaseInfo = LeaseInfo(-l.leaseIn, -l.leaseOut )

    val portfolioUpd = s.accountPortfolios
      .collect { case (acc, pf) if pf.leaseInfo != LeaseInfo.empty =>
        acc -> Portfolio(0, invertLeaseInfo(pf.leaseInfo), Map.empty)
      }

    Diff(transactions = Map.empty,
      portfolios = portfolioUpd,
      issuedAssets = Map.empty,
      slotids = Map.empty,
      addToSlot = Map.empty,
      slotNum = 0,
      txStatus = TransactionStatus.Unprocessed,
      chargedFee = 0L,
      contracts = Map.empty,
      contractDB = Map.empty,
      contractTokens = Map.empty,
      tokenDB = Map.empty,
      tokenAccountBalance = Map.empty,
      dbEntries = Map.empty,
      orderFills = Map.empty,
      leaseState = s.activeLeases().map(_ -> false).toMap)
  }

}
