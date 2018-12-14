package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.state2.{BlockDiff, ByteStr, DataTypes}
import com.wavesplatform.utils._
import kamon.Kamon
import scorex.block.Block
import scorex.transaction.History.BlockchainScore
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{HistoryWriter, Transaction, ValidationError}
import scorex.utils.{LogMVMapBuilder, ScorexLogging}
import vee.db.{ByteStrCodec, SubStorage}

import scala.util.Try
import org.iq80.leveldb.DB
import com.google.common.primitives.Ints

class HistoryWriterImpl private(db: DB, val synchronizationToken: ReentrantReadWriteLock) extends SubStorage(db, "history") with HistoryWriter {

  import HistoryWriterImpl._

  private val HeightPrefix: Array[Byte] = "height".getBytes(StandardCharsets.UTF_8)
  private val BlockBodyByHeightPrefix: Array[Byte] = "blocks".getBytes(StandardCharsets.UTF_8)
  private val BlockIdByHeightPrefix: Array[Byte] = "signatures".getBytes(StandardCharsets.UTF_8)
  private val HeightByBlockIdPrefix: Array[Byte] = "signaturesReverse".getBytes(StandardCharsets.UTF_8)
  private val ScoreByHeightPrefix:  Array[Byte] = "score".getBytes(StandardCharsets.UTF_8)


  private val blockBodyByHeight = Synchronized(db.openMap("blocks", new LogMVMapBuilder[Int, Array[Byte]]))
  private val blockIdByHeight = Synchronized(db.openMap("signatures", new LogMVMapBuilder[Int, ByteStr].valueType(DataTypes.byteStr)))
  private val heightByBlockId = Synchronized(db.openMap("signaturesReverse", new LogMVMapBuilder[ByteStr, Int].keyType(DataTypes.byteStr)))
  private val scoreByHeight = Synchronized(db.openMap("score", new LogMVMapBuilder[Int, BigInt]))

  private val blockHeightStats = Kamon.metrics.histogram("block-height")

  override def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] = write { implicit lock =>
    if ((height() == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
      val h = height() + 1
      val score = (if (height() == 0) BigInt(0) else this.score()) + block.blockScore

      put(makeKey(BlockBodyByHeightPrefix, Ints.toByteArray(h)), block.bytes, None)
      put(makeKey(ScoreByHeightPrefix, Ints.toByteArray(h)), score.toByteArray, None)
      put(makeKey(BlockIdByHeightPrefix, Ints.toByteArray(h)), ByteStrCodec.encode(block.uniqueId), None)
      put(makeKey(HeightByBlockIdPrefix, ByteStrCodec.encode(block.uniqueId)), Ints.toByteArray(h), None)
      put(makeKey(HeightPrefix, HeightPrefix), Ints.toByteArray(h), None)

      blockHeightStats.record(h)

      blockDiff
    }
    else {
      Left(GenericError(s"Parent ${block.reference} of block ${block.uniqueId} does not match last local block ${this.lastBlock.map(_.uniqueId)}"))
    }
  }

  override def discardBlock(): Seq[Transaction] = write { implicit lock =>
    val h = height()
    val transactions =
      blockBytes(h).flatmap(b => Block.parseBytes(b).fold(_ => Seq.empty[Transaction], _.transactionData.map(_.transaction)))

    delete(makeKey(BlockBodyByHeightPrefix, Ints.toByteArray(h)), None)
    delete(makeKey(ScoreByHeightPrefix, Ints.toByteArray(h)), None)
    delete(makeKey(BlockIdByHeightPrefix, Ints.toByteArray(h)), None)
    delete(makeKey(HeightByBlockIdPrefix, ByteStrCodec.encode(block.uniqueId)), None)
    put(makeKey(HeightPrefix, HeightPrefix), Ints.toByteArray(h - 1), None)

    transactions
  }


  override def lastBlockIds(howMany: Int): Seq[ByteStr] = read { implicit lock =>
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => 
      get(makeKey(BlockIdByHeightPrefix, Ints.toByteArray(height)))
      .flatMap(b => ByteStrCodec.decode(b).toOption.map(r => r.value)))
      .reverse
  }

  override def height(): Int = read { implicit lock => 
    get(makeKey(HeightPrefix, HeightPrefix)).map(Ints.fromByteArray).getOrElse(0)
  }

  override def scoreOf(id: ByteStr): Option[BlockchainScore] = read { implicit lock =>
    heightOf(id).flatMap(h => get(makeKey(ScoreByHeightPrefix, Ints.toByteArray(h))).flatMap(b => BigInt(b).toOption.map(r => r.value)))
  }

  override def heightOf(blockSignature: ByteStr): Option[Int] = read { implicit lock =>
    get(makeKey(HeightByBlockIdPrefix, ByteStrCodec.encode(blockSignature))).map(Ints.fromByteArray)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit lock =>
    get(makeKey(BlockBodyByHeightPrefix, Ints.toByteArray(height)))
  }

}

object HistoryWriterImpl extends ScorexLogging {

  def apply(db: DB, synchronizationToken: ReentrantReadWriteLock): Try[HistoryWriterImpl] =
    new HistoryWriterImpl(db, synchronizationToken)
}
