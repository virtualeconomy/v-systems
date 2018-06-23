package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.{CreateContractTransaction, ValidationError}

import scala.util.Right

object CreateContractTransactionDiff {
  def apply(height: Int)(tx: CreateContractTransaction): Either[ValidationError, Diff] = {
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      contracts = Map(tx.contract.name -> tx.contract.content)
    ))
  }
}
