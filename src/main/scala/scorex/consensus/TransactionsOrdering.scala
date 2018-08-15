package scorex.consensus

import scorex.transaction.Transaction

object TransactionsOrdering {
  trait VEEOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Short, Long, Long, String) = {
      val byFeeScale: Short = (-t.assetFee._3).toShort
      val byFee = if (t.assetFee._1.nonEmpty) 0 else -t.assetFee._2
      val byTimestamp = txTimestampOrder(t.timestamp)
      val byTxId = t.id.base58

      (byFeeScale, byFee, byTimestamp, byTxId)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Short, Long, Long, String)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends VEEOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends VEEOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
