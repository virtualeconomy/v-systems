package vee.db

import java.util.concurrent.locks.ReentrantReadWriteLock
import com.google.common.cache.CacheBuilder
import scorex.account.Address
import scorex.block.Block
import vee.transaction.ProcessedTransaction
import vee.transaction.proof.EllipticCurve25519Proof
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.LeaseDetails
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

object LevelDBWriter {

  val MAX_DEPTH = 2000

  private def loadLeaseStatus(db: ReadOnlyDB, leaseId: ByteStr): Boolean =
    db.get(Keys.leaseStatusHistory(leaseId)).headOption.fold(false)(h => db.get(Keys.leaseStatus(leaseId)(h)))

  /** {{{
    * ([10, 7, 4], 5, 11) => [10, 7, 4]
    * ([10, 7], 5, 11) => [10, 7, 1]
    * }}}
    */
  private[db] def slice(v: Seq[Int], from: Int, to: Int): Seq[Int] = {
    val (c1, c2) = v.dropWhile(_ > to).partition(_ > from)
    c1 :+ c2.headOption.getOrElse(1)
  }

  /** {{{([15, 12, 3], [12, 5]) => [(15, 12), (12, 12), (3, 12), (3, 5)]}}}
    *
    * @param vbh VEE balance history
    * @param lbh Lease balance history
    */
  private[db] def merge(vbh: Seq[Int], lbh: Seq[Int]): Seq[(Int, Int)] = {
    @tailrec
    def recMerge(wh: Int, wt: Seq[Int], lh: Int, lt: Seq[Int], buf: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = {
      buf += wh -> lh
      if (wt.isEmpty && lt.isEmpty) {
        buf
      } else if (wt.isEmpty) {
        recMerge(wh, wt, lt.head, lt.tail, buf)
      } else if (lt.isEmpty) {
        recMerge(wt.head, wt.tail, lh, lt, buf)
      } else {
        if (wh >= lh) {
          recMerge(wt.head, wt.tail, lh, lt, buf)
        } else {
          recMerge(wh, wt, lt.head, lt.tail, buf)
        }
      }
    }

    recMerge(vbh.head, vbh.tail, lbh.head, lbh.tail, ArrayBuffer.empty)
  }

  implicit class ReadOnlyDBExt(val db: ReadOnlyDB) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))

    def hasInHistory(historyKey: Key[Seq[Int]], v: Int => Key[_]): Boolean =
      db.get(historyKey)
        .headOption
        .exists(h => db.has(v(h)))
  }

  implicit class RWExt(val db: RW) extends AnyVal {
    def fromHistory[A](historyKey: Key[Seq[Int]], valueKey: Int => Key[A]): Option[A] =
      for {
        lastChange <- db.get(historyKey).headOption
      } yield db.get(valueKey(lastChange))
  }
}

class LevelDBWriter(writableDB: DB, fs: FunctionalitySettings, val maxCacheSize: Int = 100000,
                    val synchronizationToken: ReentrantReadWriteLock) extends Caches with ScorexLogging {

  import LevelDBWriter._

  private def readOnly[A](f: ReadOnlyDB => A): A = writableDB.readOnly(f)
  private def readWrite[A](f: RW => A): A        = writableDB.readWrite(f)

  override def close(): Unit = writableDB.close()

  override protected def loadMaxAddressId(): BigInt = readOnly(db => db.get(Keys.lastAddressId).getOrElse(BigInt(0)))

  override protected def loadAddressId(address: Address): Option[BigInt] = readOnly(db => db.get(Keys.addressId(address)))

  override protected def loadHeight(): Int = readOnly(_.get(Keys.height))

  override protected def loadScore(): BigInt = readOnly(db => db.get(Keys.score(db.get(Keys.height))))

  override protected def loadLastBlock(): Option[Block] = readOnly(db => db.get(Keys.blockAt(db.get(Keys.height))))

  private def loadSposPortfolio(db: ReadOnlyDB, addressId: BigInt) = Portfolio(
    db.fromHistory(Keys.veeBalanceHistory(addressId), Keys.veeBalance(addressId)).getOrElse(0L),
    db.fromHistory(Keys.leaseBalanceHistory(addressId), Keys.leaseBalance(addressId)).getOrElse(LeaseInfo.empty),
    Map.empty
  )

  override protected def loadPortfolio(address: Address): Portfolio = readOnly { db =>
    addressId(address).fold(Portfolio.empty)(loadSposPortfolio(db, _))
  }

  private def updateHistory(rw: RW, key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] =
    updateHistory(rw, rw.get(key), key, threshold, kf)

  private def updateHistory(rw: RW, history: Seq[Int], key: Key[Seq[Int]], threshold: Int, kf: Int => Key[_]): Seq[Array[Byte]] = {
    val (c1, c2) = history.partition(_ > threshold)
    rw.put(key, (height +: c1) ++ c2.headOption)
    c2.drop(1).map(kf(_).keyBytes)
  }

  override protected def doAppend(block: Block,
                                  newAddresses: Map[Address, BigInt],
                                  veeBalances: Map[BigInt, Long],
                                  leaseBalances: Map[BigInt, LeaseInfo],
                                  leaseStates: Map[ByteStr, Boolean],
                                  transactions: Map[ByteStr, (ProcessedTransaction, Set[BigInt])],
                                  addressTransactions: Map[BigInt, List[ByteStr]]
                                  ): Unit = readWrite { rw =>
    val expiredKeys = new ArrayBuffer[Array[Byte]]

    rw.put(Keys.height, height)
    rw.put(Keys.blockAt(height), Some(block))
    rw.put(Keys.heightOf(block.uniqueId), Some(height))
    rw.put(Keys.lastAddressId, Some(loadMaxAddressId() + newAddresses.size))
    rw.put(Keys.score(height), rw.get(Keys.score(height - 1)) + block.blockScore)

    for ((address, id) <- newAddresses) {
      rw.put(Keys.addressId(address), Some(id))
      rw.put(Keys.idToAddress(id), address)
    }

    val threshold = height - MAX_DEPTH

    val newAddressesForVee = ArrayBuffer.empty[BigInt]
    val updatedBalanceAddresses = for ((addressId, balance) <- veeBalances) yield {
      val kwbh = Keys.veeBalanceHistory(addressId)
      val wbh  = rw.get(kwbh)
      if (wbh.isEmpty) {
        newAddressesForVee += addressId
      }
      rw.put(Keys.veeBalance(addressId)(height), balance)
      expiredKeys ++= updateHistory(rw, wbh, kwbh, threshold, Keys.veeBalance(addressId))
      addressId
    }

    val changedAddresses = addressTransactions.keys ++ updatedBalanceAddresses

    if (newAddressesForVee.nonEmpty) {
      val newSeqNr = rw.get(Keys.addressesForVeeSeqNr) + 1
      rw.put(Keys.addressesForVeeSeqNr, newSeqNr)
      rw.put(Keys.addressesForVee(newSeqNr), newAddressesForVee)
    }

    for ((addressId, leaseBalance) <- leaseBalances) {
      rw.put(Keys.leaseBalance(addressId)(height), leaseBalance)
      expiredKeys ++= updateHistory(rw, Keys.leaseBalanceHistory(addressId), threshold, Keys.leaseBalance(addressId))
    }

    rw.put(Keys.changedAddresses(height), changedAddresses.toSeq)

    for ((leaseId, state) <- leaseStates) {
      rw.put(Keys.leaseStatus(leaseId)(height), state)
      expiredKeys ++= updateHistory(rw, Keys.leaseStatusHistory(leaseId), threshold, Keys.leaseStatus(leaseId))
    }

    for ((addressId, txs) <- addressTransactions) {
      val kk        = Keys.addressTransactionSeqNr(addressId)
      val nextSeqNr = rw.get(kk) + 1
      rw.put(Keys.addressTransactionIds(addressId, nextSeqNr), txs)
      rw.put(kk, nextSeqNr)
    }

    for ((id, (tx, _)) <- transactions) {
      rw.put(Keys.transactionInfo(id), Some((height, tx)))
    }

    rw.put(Keys.transactionIdsAtHeight(height), transactions.keys.toSeq)

    expiredKeys.foreach(rw.delete(_, "expired-keys"))
  }

  override protected def doRollback(targetBlockId: ByteStr): Seq[Block] = {
    readOnly(_.get(Keys.heightOf(targetBlockId))).fold(Seq.empty[Block]) { targetHeight =>
      log.debug(s"Rolling back to block $targetBlockId at $targetHeight")

      val discardedBlocks: Seq[Block] = for (currentHeight <- height until targetHeight by -1) yield {
        val portfoliosToInvalidate = Seq.newBuilder[Address]

        val discardedBlock = readWrite { rw =>
          log.trace(s"Rolling back to ${currentHeight - 1}")
          rw.put(Keys.height, currentHeight - 1)

          for (addressId <- rw.get(Keys.changedAddresses(currentHeight))) {
            val address = rw.get(Keys.idToAddress(addressId))

            rw.delete(Keys.leaseBalance(addressId)(currentHeight))
            rw.filterHistory(Keys.leaseBalanceHistory(addressId), currentHeight)

            log.trace(s"Discarding portfolio for $address")

            portfoliosToInvalidate += address
            balanceAtHeightCache.invalidate((currentHeight, addressId))
            leaseBalanceAtHeightCache.invalidate((currentHeight, addressId))

            val kTxSeqNr = Keys.addressTransactionSeqNr(addressId)
            val txSeqNr  = rw.get(kTxSeqNr)
            val kTxIds   = Keys.addressTransactionIds(addressId, txSeqNr)
            for (id <- rw.get(kTxIds).headOption; (h, _) <- rw.get(Keys.transactionInfo(id)) if h == currentHeight) {
              rw.delete(kTxIds)
              rw.put(kTxSeqNr, (txSeqNr - 1).max(0))
            }
          }

          val txIdsAtHeight = Keys.transactionIdsAtHeight(currentHeight)
          for (txId <- rw.get(txIdsAtHeight)) {
            forgetTransaction(txId)
            val ktxId = Keys.transactionInfo(txId)

            for ((_, ptx) <- rw.get(ktxId)) {
              val tx = ptx.transaction
              tx match {
                case _: GenesisTransaction                                                       => // genesis transaction can not be rolled back
                case _: PaymentTransaction | _: TransferTransaction /*| _: MassTransferTransaction*/ =>
                // balances already restored
                case tx: LeaseTransaction =>
                  rollbackLeaseStatus(rw, tx.id, currentHeight)
                case tx: LeaseCancelTransaction =>
                  rollbackLeaseStatus(rw, tx.leaseId, currentHeight)
              }
            }

            rw.delete(ktxId)
          }

          rw.delete(txIdsAtHeight)

          val discardedBlock = rw
            .get(Keys.blockAt(currentHeight))
            .getOrElse(throw new IllegalArgumentException(s"No block at height $currentHeight"))

          rw.delete(Keys.blockAt(currentHeight))
          rw.delete(Keys.heightOf(discardedBlock.uniqueId))

          discardedBlock
        }

        portfoliosToInvalidate.result().foreach(discardPortfolio)
        discardedBlock
      }

      log.debug(s"Rollback to block $targetBlockId at $targetHeight completed")

      discardedBlocks.reverse
    }
  }

  private def rollbackLeaseStatus(rw: RW, leaseId: ByteStr, currentHeight: Int): Unit = {
    rw.delete(Keys.leaseStatus(leaseId)(currentHeight))
    rw.filterHistory(Keys.leaseStatusHistory(leaseId), currentHeight)
  }

  override def transactionInfo(id: ByteStr): Option[(Int, ProcessedTransaction)] = readOnly(db => db.get(Keys.transactionInfo(id)))

  override def transactionHeight(id: ByteStr): Option[Int] = readOnly(db => db.get(Keys.transactionHeight(id)))

  /*
  // Use accountTransactionIds() instead for current version
  override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, ProcessedTransaction)] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq.empty[(Int, ProcessedTransaction)]) { addressId =>
      val txs = for {
        seqNr          <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
        (txType, txId) <- db.get(Keys.addressTransactionIds(addressId, seqNr))
        if types.isEmpty || types.contains(txType.toByte)
        (h, tx) <- db.get(Keys.transactionInfo(txId))
      } yield (h, tx)

      txs.slice(from, count).force
    }
  }
  */

  override def accountTransactionIds(address: Address, limit: Int): Seq[ByteStr] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq.empty[ByteStr]) { addressId =>
      val txs = for {
        seqNr          <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
        txId <- db.get(Keys.addressTransactionIds(addressId, seqNr))
      } yield txId
      txs.takeRight(limit).force
    }
  }

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = readOnly { db =>
    val tx = db.get(Keys.transactionInfo(leaseId)).getOrElse(throw new IllegalArgumentException(s"No lease transaction found for $leaseId"))
    tx._2.transaction match {
      case lt: LeaseTransaction =>
        val publicKey = EllipticCurve25519Proof.fromBytes(lt.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        Some(LeaseDetails(publicKey, lt.recipient, tx._1, lt.amount, loadLeaseStatus(db, leaseId)))
      case _ => None
    }
  }

  // These two caches are used exclusively for balance snapshots. They are not used for portfolios, because there aren't
  // as many miners, so snapshots will rarely be evicted due to overflows.

  private val balanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, BigInt), java.lang.Long]()

  private val leaseBalanceAtHeightCache = CacheBuilder
    .newBuilder()
    .maximumSize(100000)
    .recordStats()
    .build[(Int, BigInt), LeaseInfo]()

  //Todo: Implement balanceSnapshots later
  /*
  override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = readOnly { db =>
    db.get(Keys.addressId(address)).fold(Seq(BalanceSnapshot(1, 0, 0, 0))) { addressId =>
      val wbh = slice(db.get(Keys.veeBalanceHistory(addressId)), from, to)
      val lbh = slice(db.get(Keys.leaseBalanceHistory(addressId)), from, to)
      for {
        (wh, lh) <- merge(wbh, lbh)
        wb = balanceAtHeightCache.get((wh, addressId), () => db.get(Keys.veeBalance(addressId)(wh)))
        lb = leaseBalanceAtHeightCache.get((lh, addressId), () => db.get(Keys.leaseBalance(addressId)(lh)))
      } yield BalanceSnapshot(wh.max(lh), wb, lb.leaseIn, lb.leaseOut)
    }
  }
  */

  override def activeLeases(): Seq[ByteStr] = readOnly { db =>
    val txs = for {
      h  <- 1 to db.get(Keys.height)
      id <- db.get(Keys.transactionIdsAtHeight(h))
      if loadLeaseStatus(db, id)
      (_, tx) <- db.get(Keys.transactionInfo(id))
    } yield tx.transaction

    txs.collect { case lt: LeaseTransaction => lt.id }
  }

  override def collectSposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = readOnly { db =>
    val b = Map.newBuilder[Address, A]
    for (id <- BigInt(1) to db.get(Keys.lastAddressId).getOrElse(BigInt(0))) {
      val address = db.get(Keys.idToAddress(id))
      pf.runWith(b += address -> _)(address -> loadSposPortfolio(db, id))
    }
    b.result()
  }

  override def scoreOf(blockId: ByteStr): Option[BigInt] = readOnly(db => db.get(Keys.heightOf(blockId)).map(h => db.get(Keys.score(h))))

  override def blockBytes(height: Int): Option[Array[Byte]] = readOnly(_.get(Keys.blockBytes(height)))

  override def blockBytes(blockId: ByteStr): Option[Array[Byte]] =
    readOnly(db => db.get(Keys.heightOf(blockId)).flatMap(h => db.get(Keys.blockBytes(h))))

  override def heightOf(blockId: ByteStr): Option[Int] = readOnly(_.get(Keys.heightOf(blockId)))

  override def lastBlockIds(howMany: Int): immutable.IndexedSeq[ByteStr] = readOnly { db =>
    // since this is called from outside of the main blockchain updater thread, instead of using cached height,
    // explicitly read height from storage to make this operation atomic.
    val currentHeight = db.get(Keys.height)
    (currentHeight until (currentHeight - howMany).max(0) by -1)
      .map(h => db.get(Keys.blockAt(h)).get.signerData.signature)
  }

  override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = readOnly { db =>
    db.get(Keys.heightOf(parentSignature)).map { parentHeight =>
      (parentHeight until (parentHeight + howMany))
        .flatMap { h =>
          db.get(Keys.blockAt(h))
        }
        .map { b =>
          b.signerData.signature
        }
    }
  }

  override def parent(block: Block, back: Int): Option[Block] = readOnly { db =>
    db.get(Keys.heightOf(block.reference)).flatMap(h => db.get(Keys.blockAt(h - back + 1)))
  }

  override def veeDistribution(height: Int): Map[Address, Long] = readOnly { db =>
    (for {
      seqNr     <- (1 to db.get(Keys.addressesForVeeSeqNr)).par
      addressId <- db.get(Keys.addressesForVee(seqNr)).par
      history = db.get(Keys.veeBalanceHistory(addressId))
      actualHeight <- history.partition(_ > height)._2.headOption
      balance = db.get(Keys.veeBalance(addressId)(actualHeight))
      if balance > 0
    } yield db.get(Keys.idToAddress(addressId)) -> balance).toMap.seq
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = {
    //TODO: StateReader
    throw new NotImplementedError()
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = {
    //TODO: StateReader
    throw new NotImplementedError()
  }

  override def lastUpdateWeightedBalance(acc: Address): Option[Long] = {
    //TODO: StateReader
    throw new NotImplementedError()
  }

  override def slotAddress(id: Int): Option[String] = readOnly {
    //TODO: SPOS
    throw new NotImplementedError()
  }

  override def addressToSlotID(add: String): Option[Int] = readOnly {
    //TODO: SPOS
    throw new NotImplementedError()
  }

  override def effectiveSlotAddressSize: Int = {
    //TODO: SPOS
    throw new NotImplementedError()
  }

  override def dbGet(key: ByteStr): Option[ByteStr] = readOnly {
    //TODO: VEE
    throw new NotImplementedError()
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = {
    //TODO: VEE
    throw new NotImplementedError()
  }

  override def contractContent(name: String): Option[(Boolean, ByteStr, String)] = {
    throw new NotImplementedError() // not implemented for this release
  }
}
