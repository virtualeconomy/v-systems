package vee.db

import com.google.common.primitives.{Ints, Longs}
import scorex.account.Address
import scorex.block.Block
import com.wavesplatform.state2._
import vee.transaction.ProcessedTransaction

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)
  def score(height: Int): Key[BigInt] = Key("score", h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  private def blockAtHeight(height: Int) = h(3, height)

  def blockAt(height: Int): Key[Option[Block]]                  = Key.opt[Block]("block-at", blockAtHeight(height), Block.parseBytes(_).get, _.bytes)
  def blockBytes(height: Int): Key[Option[Array[Byte]]]         = Key.opt[Array[Byte]]("block-bytes", blockAtHeight(height), identity, identity)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int]("height-of", hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def veeBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("vee-balance-history", 5, addressId.toByteArray)
  def veeBalance(addressId: BigInt)(height: Int): Key[Long] =
  Key("vee-balance", hAddr(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("lease-balance-history", 12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseInfo] =
    Key("lease-balance", hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey("lease-status-history", 14, leaseId.arr)
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def transactionInfo(txId: ByteStr): Key[Option[(Int, ProcessedTransaction)]] =
    Key.opt("transaction-info", hash(18, txId), readTransactionInfo, writeTransactionInfo)
  def transactionHeight(txId: ByteStr): Key[Option[Int]] =
    Key.opt("transaction-height", hash(18, txId), readTransactionHeight, unsupported("Can't write transaction height only"))

  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key("changed-addresses", h(21, height), readBigIntSeq, writeBigIntSeq)

  def transactionIdsAtHeight(height: Int): Key[Seq[ByteStr]] = Key("transaction-ids-at-height", h(22, height), readTxIds, writeTxIds)

  val lastAddressId: Key[Option[BigInt]] = Key.opt("last-address-id", Array[Byte](0, 24), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt("address-id", bytes(25, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key("id-to-address", bytes(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  val addressesForVeeSeqNr: Key[Int]                = intKey("addresses-for-vee-seq-nr", 37)
  def addressesForVee(seqNr: Int): Key[Seq[BigInt]] = Key("addresses-for-vee", h(38, seqNr), readBigIntSeq, writeBigIntSeq)

  def addressTransactionSeqNr(addressId: BigInt): Key[Int] = bytesSeqNr("address-transaction-seq-nr", 41, addressId.toByteArray)
  def addressTransactionIds(addressId: BigInt, seqNr: Int): Key[Seq[ByteStr]] =
    //Key("address-transaction-ids", hBytes(42, seqNr, addressId.toByteArray), readTransactionIds, writeTransactionIds)
    Key("address-transaction-ids", hBytes(42, seqNr, addressId.toByteArray), readTxIds, writeTxIds)

  def snapshot(addressId: BigInt)(height: Int): Key[Long] =
    Key("vee-snapshot", hAddr(50, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

}
