package vsys.blockchain.state.opcdiffs

import cats.Monoid
import cats.implicits._
import vsys.blockchain.state.{BlockDiff, ByteStr, Diff, Portfolio}
import vsys.blockchain.transaction.Transaction
import vsys.account.{Account, Address}

case class OpcDiff(contractDB: Map[ByteStr, Array[Byte]] = Map.empty,
                   contractNumDB: Map[ByteStr, Long] = Map.empty,
                   contractStateDB: Map[ByteStr, Boolean] = Map.empty,
                   contractTokens: Map[ByteStr, Int] = Map.empty,
                   tokenDB: Map[ByteStr, Array[Byte]] = Map.empty,
                   tokenAccountBalance: Map[ByteStr, Long] = Map.empty,
                   relatedAddress: Map[Address, Boolean] = Map.empty,
                   portfolios: Map[Account, Portfolio] = Map.empty
                  ) {

}

object OpcDiff {

  val empty = new OpcDiff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit class OpcDiffExt(d: OpcDiff) {
    def asTransactionDiff(height: Int, tx: Transaction): Diff =
      Diff(height              = height,
           tx                  = tx,
           contractDB          = d.contractDB,
           contractNumDB       = d.contractNumDB,
           contractStateDB     = d.contractStateDB,
           contractTokens      = d.contractTokens,
           tokenDB             = d.tokenDB,
           tokenAccountBalance = d.tokenAccountBalance,
           relatedAddress      = d.relatedAddress,
           portfolios = d.portfolios
      )

    def asBlockDiff(height: Int, tx: Transaction): BlockDiff =
      BlockDiff(d.asTransactionDiff(height, tx), 0, Map.empty)

  }

  implicit val opcDiffMonoid = new Monoid[OpcDiff] {
    override def empty: OpcDiff = OpcDiff.empty

    override def combine(older: OpcDiff, newer: OpcDiff): OpcDiff =
      OpcDiff(
        older.contractDB ++ newer.contractDB,
        Monoid.combine(older.contractNumDB,newer.contractNumDB),
        older.contractStateDB ++ newer.contractStateDB,
        Monoid.combine(older.contractTokens,newer.contractTokens),
        older.tokenDB ++ newer.tokenDB,
        Monoid.combine(older.tokenAccountBalance,newer.tokenAccountBalance),
        older.relatedAddress ++ newer.relatedAddress,
        older.portfolios.combine(newer.portfolios)
      )
  }
}
