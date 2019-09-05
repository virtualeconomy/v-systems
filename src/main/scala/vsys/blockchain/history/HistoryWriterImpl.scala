package vsys.blockchain.history

import java.util.concurrent.locks.ReentrantReadWriteLock
import java.nio.charset.StandardCharsets

import com.google.common.primitives.Ints
import kamon.Kamon
import org.iq80.leveldb.DB
import vsys.blockchain.block.Block
import vsys.blockchain.history.History.BlockchainScore
import vsys.blockchain.state.{BlockDiff, ByteStr}
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.{Transaction, ValidationError}
import vsys.db.{ByteStrCodec, SubStorage}

class HistoryWriterImpl(db: DB, val synchronizationToken: ReentrantReadWriteLock, renew: Boolean =false) extends SubStorage(db, "history") with HistoryWriter {

  private val HeightPrefix: Array[Byte] = "height".getBytes(StandardCharsets.UTF_8)
  private val BlockBodyByHeightPrefix: Array[Byte] = "blocks".getBytes(StandardCharsets.UTF_8)
  private val BlockIdByHeightPrefix: Array[Byte] = "signatures".getBytes(StandardCharsets.UTF_8)
  private val HeightByBlockIdPrefix: Array[Byte] = "signaturesReverse".getBytes(StandardCharsets.UTF_8)
  private val ScoreByHeightPrefix:  Array[Byte] = "score".getBytes(StandardCharsets.UTF_8)

  private val blockHeightStats = Kamon.metrics.histogram("block-height")

  private def blockBodyByHeightKey(height: Int): Array[Byte] = makeKey(BlockBodyByHeightPrefix, Ints.toByteArray(height))
  private def blockIdByHeightKey(height: Int): Array[Byte] = makeKey(BlockIdByHeightPrefix, Ints.toByteArray(height))
  private def heightByBlockIdKey(blockId: ByteStr): Array[Byte] = makeKey(HeightByBlockIdPrefix, ByteStrCodec.encode(blockId))
  private def scoreByHeightKey(height: Int): Array[Byte] = makeKey(ScoreByHeightPrefix, Ints.toByteArray(height))
  private val heightKey: Array[Byte] = makeKey(HeightPrefix, HeightPrefix)
  
  override def appendBlock(block: Block)(consensusValidation: => Either[ValidationError, BlockDiff]): Either[ValidationError, BlockDiff] = write { implicit lock =>
    if ((height() == 0) || (this.lastBlock.get.uniqueId == block.reference)) consensusValidation.map { blockDiff =>
      val h = height() + 1
      val score = (if (height() == 0) BigInt(0) else this.score()) + block.blockScore

      val batch = createBatch()
      put(blockBodyByHeightKey(h), block.bytes, batch)
      put(scoreByHeightKey(h), score.toByteArray, batch)
      put(blockIdByHeightKey(h), ByteStrCodec.encode(block.uniqueId), batch)
      put(heightByBlockIdKey(block.uniqueId), Ints.toByteArray(h), batch)
      put(heightKey, Ints.toByteArray(h), batch)
      commit(batch)

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
      Block.parseBytes(blockBytes(h).getOrElse(Array[Byte]())).fold(_ => Seq.empty[Transaction], _.transactionData.map(_.transaction))

    val batch = createBatch()
    delete(blockBodyByHeightKey(h), batch)
    delete(scoreByHeightKey(h), batch)
    get(blockIdByHeightKey(h)).foreach(b => ByteStrCodec.decode(b).toOption.foreach(r => delete(heightByBlockIdKey(r.value), batch)))
    delete(blockIdByHeightKey(h), batch)
    put(heightKey, Ints.toByteArray(h - 1), batch)
    commit(batch)


    transactions
  }


  override def lastBlockIds(howMany: Int): Seq[ByteStr] = read { implicit lock =>
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => 
      get(blockIdByHeightKey(i))
      .flatMap(b => ByteStrCodec.decode(b).toOption.map(r => r.value)))
      .reverse
  }

  override def height(): Int = read { implicit lock => 
    get(heightKey).map(Ints.fromByteArray).get
  }

  override def scoreOf(id: ByteStr): Option[BlockchainScore] = read { implicit lock =>
    heightOf(id).map(h => get(scoreByHeightKey(h)).map(b => BigInt(b)).getOrElse(BigInt(0)))
  }

  override def heightOf(blockSignature: ByteStr): Option[Int] = read { implicit lock =>
    get(heightByBlockIdKey(blockSignature)).map(Ints.fromByteArray)
  }

  override def blockBytes(height: Int): Option[Array[Byte]] = read { implicit lock =>
    get(blockBodyByHeightKey(height))
  }

  if (renew || get(heightKey).isEmpty) put(heightKey, Ints.toByteArray(0), None)

}
