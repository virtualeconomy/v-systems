package vsys.state.opcdiffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.{BlockDiff, ByteStr, Diff}
import scorex.transaction.Transaction

case class OpcDiff(contractDB: Map[ByteStr, Array[Byte]],
                   contractTokens: Map[ByteStr, Int],
                   tokenDB: Map[ByteStr, Array[Byte]],
                   tokenAccountBalance: Map[ByteStr, Long]) {

}

object OpcDiff {
  def apply(contractDB: Map[ByteStr, Array[Byte]] = Map.empty,
            contractTokens: Map[ByteStr, Int] = Map.empty,
            tokenDB: Map[ByteStr, Array[Byte]] = Map.empty,
            tokenAccountBalance: Map[ByteStr, Long] = Map.empty): OpcDiff = new OpcDiff(
    contractDB = contractDB,
    contractTokens = contractTokens,
    tokenDB = tokenDB,
    tokenAccountBalance = tokenAccountBalance)

  val empty = new OpcDiff(Map.empty, Map.empty, Map.empty, Map.empty)

  implicit class OpcDiffExt(d: OpcDiff) {
    def asTransactionDiff(height: Int, tx: Transaction): Diff = Diff(height = height, tx = tx,
                                                                     contractDB = d.contractDB,
                                                                     contractTokens = d.contractTokens,
                                                                     tokenAccountBalance = d.tokenAccountBalance
    )
    def asBlockDiff(height: Int, tx: Transaction): BlockDiff = BlockDiff(d.asTransactionDiff(height, tx), 0, Map.empty)

  }

  implicit val opcDiffMonoid = new Monoid[OpcDiff] {
    override def empty: OpcDiff = OpcDiff.empty

    override def combine(older: OpcDiff, newer: OpcDiff): OpcDiff = OpcDiff(
      contractDB = older.contractDB ++ newer.contractDB,
      contractTokens = Monoid.combine(older.contractTokens, newer.contractTokens),
      tokenDB = newer.contractDB,
      tokenAccountBalance = Monoid.combine(older.tokenAccountBalance, newer.tokenAccountBalance)
    )
  }
}