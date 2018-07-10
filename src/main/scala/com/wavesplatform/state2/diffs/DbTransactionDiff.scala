package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import scorex.transaction.database.DbPutTransaction

import scala.util.Right

object DbTransactionDiff {
  def put(s: StateReader, height: Int)(tx: DbPutTransaction): Either[ValidationError, Diff] = {
    // any validation needed? maybe later access control?
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      dbEntries = Map(tx.storageKey -> tx.entry)
    ))
  }

}
