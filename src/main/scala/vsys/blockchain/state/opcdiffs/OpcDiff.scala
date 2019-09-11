package vsys.blockchain.state.opcdiffs

import cats.Monoid
import cats.implicits._
import vsys.blockchain.state.{BlockDiff, ByteStr, Diff}
import vsys.blockchain.transaction.Transaction
import vsys.account.Address

case class OpcDiff(contractDB: Map[ByteStr, Array[Byte]] = Map.empty,
                   contractTokens: Map[ByteStr, Int] = Map.empty,
                   tokenDB: Map[ByteStr, Array[Byte]] = Map.empty,
                   tokenAccountBalance: Map[ByteStr, Long] = Map.empty,
                   relatedAddress: Map[Address, Boolean] = Map.empty) {

}

object OpcDiff {

  val empty = new OpcDiff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit class OpcDiffExt(d: OpcDiff) {
    def asTransactionDiff(height: Int, tx: Transaction): Diff =
      Diff(height              = height,
           tx                  = tx,
           contractDB          = d.contractDB,
           contractTokens      = d.contractTokens,
           tokenDB             = d.tokenDB,
           tokenAccountBalance = d.tokenAccountBalance,
           relatedAddress      = d.relatedAddress
      )

    def asBlockDiff(height: Int, tx: Transaction): BlockDiff =
      BlockDiff(d.asTransactionDiff(height, tx), 0, Map.empty)

  }

  implicit val opcDiffMonoid = new Monoid[OpcDiff] {
    override def empty: OpcDiff = OpcDiff.empty

    override def combine(older: OpcDiff, newer: OpcDiff): OpcDiff =
      OpcDiff(
        contractDB          = older.contractDB ++ newer.contractDB,
        contractTokens      = Monoid.combine(older.contractTokens, newer.contractTokens),
        tokenDB             = older.tokenDB ++ newer.tokenDB,
        tokenAccountBalance = Monoid.combine(older.tokenAccountBalance,newer.tokenAccountBalance),
        relatedAddress      = older.relatedAddress ++ newer.relatedAddress
      )
  }
}
