package vsys.state.opcdiffs

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.ByteStr

case class OpcDiff(contractDB: Map[ByteStr, Array[Byte]],
                   contractTokens: Map[ByteStr, Int],
                   tokenAccountBalance: Map[ByteStr, Long]) {

}

object OpcDiff {
  def apply(contractDB: Map[ByteStr, Array[Byte]] = Map.empty,
            contractTokens: Map[ByteStr, Int] = Map.empty,
            tokenAccountBalance: Map[ByteStr, Long] = Map.empty): OpcDiff = new OpcDiff(
    contractDB = contractDB,
    contractTokens = contractTokens,
    tokenAccountBalance = tokenAccountBalance)

  val empty = new OpcDiff(Map.empty, Map.empty, Map.empty)

  implicit val opcDiffMonoid = new Monoid[OpcDiff] {
    override def empty: OpcDiff = OpcDiff.empty

    override def combine(older: OpcDiff, newer: OpcDiff): OpcDiff = OpcDiff(
      contractDB = older.contractDB ++ newer.contractDB,
      contractTokens = Monoid.combine(older.contractTokens, newer.contractTokens),
      tokenAccountBalance = Monoid.combine(older.tokenAccountBalance, newer.tokenAccountBalance)
    )
  }
}