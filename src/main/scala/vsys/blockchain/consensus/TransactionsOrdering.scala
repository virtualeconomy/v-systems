package vsys.blockchain.consensus

import vsys.blockchain.transaction.Transaction

object TransactionsOrdering {
  trait VSYSOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Short, Long, Long, String) = {
      val byFeeScale: Short = (-t.feeScale).toShort
      val byFee = -t.transactionFee
      val byTimestamp = txTimestampOrder(t.timestamp)
      val byTxId = t.id.base58

      (byFeeScale, byFee, byTimestamp, byTxId)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Short, Long, Long, String)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends VSYSOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends VSYSOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
