package vsys.db

import java.nio.charset.{Charset, StandardCharsets}

import com.google.common.primitives.{Bytes, Ints, UnsignedBytes}
import com.wavesplatform.utils.forceStopApplication
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}
import scorex.utils.ScorexLogging

import scala.collection.AbstractIterator
import scala.util.control.NonFatal

abstract class Storage(private val db: DB) extends ScorexLogging {
  protected val Charset: Charset = StandardCharsets.UTF_8

  protected val Separator: Array[Byte] = Array[Byte](':')

  def get(key: Array[Byte]): Option[Array[Byte]] = {
    try {
      Option(db.get(key))
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB get error", t)
        None
    }
  }

  def createBatch(): Option[WriteBatch] = {
    try {
      Some(db.createWriteBatch())
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB create batch error", t)
        forceStopApplication()
        throw t
    }
  }

  def put(key: Array[Byte], value: Array[Byte], batch: Option[WriteBatch]): Unit = {
    try {
      if (batch.isDefined) batch.get.put(key, value) else db.put(key, value)
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB batch put error", t)
        forceStopApplication()
        throw t
    }
  }

  def delete(key: Array[Byte], batch: Option[WriteBatch]): Unit = {
    try {
      if (batch.isDefined) batch.get.delete(key) else db.delete(key)
    } catch {
      case NonFatal(t) =>
        log.error("LevelDB delete error", t)
        forceStopApplication()
        throw t
    }
  }

  def commit(batch: Option[WriteBatch]): Unit = {
    batch.foreach { b =>
      try {
        db.write(b)
      } catch {
        case NonFatal(t) =>
          log.error("LevelDB write batch error", t)
          forceStopApplication()
          throw t
      } finally {
        b.close()
      }
    }
  }

  class DBPrefixIterator(val it: DBIterator, val prefix: Option[Array[Byte]]) extends AbstractIterator[(Array[Byte], Array[Byte])] {

    if (prefix.isEmpty) it.seekToFirst() else it.seek(prefix.get)

    override def hasNext: Boolean = it.hasNext && (prefix.isEmpty || it.peekNext().getKey().startsWith(prefix.get))

    override def next(): (Array[Byte], Array[Byte]) = {
      val entry = it.next()
      (entry.getKey, entry.getValue)
    }

    def nextKey(): Array[Byte] = it.next().getKey

    def close(): Unit = it.close()
  }

  class DBRangeIterator(val it: DBIterator,
                        val low: Option[Array[Byte]],
                        val high: Option[Array[Byte]],
                        val includeLow: Boolean = true,
                        val includeHigh: Boolean = true) extends AbstractIterator[(Array[Byte], Array[Byte])] {

    private def byteCmp(a: Array[Byte], b: Array[Byte]): Int = UnsignedBytes.lexicographicalComparator().compare(a, b)

    private def rangeContainsNextKey: Boolean = {
      val nextKey = it.peekNext().getKey()
      val cmpLow = () => byteCmp(low.get, nextKey)
      val cmpHigh = () => byteCmp(nextKey, high.get)
      (low.isEmpty || cmpLow() < 0 || (includeLow && cmpLow() == 0)) && (high.isEmpty || cmpHigh() < 0 || (includeHigh && cmpHigh() == 0))
    }

    if (low.isEmpty) {
      it.seekToFirst()
    } else {
      it.seek(low.get)
      // skip the next if the includeLow is false and the db contains low
      if (it.hasNext && !rangeContainsNextKey) it.next()
    }

    override def hasNext: Boolean = it.hasNext && rangeContainsNextKey

    override def next(): (Array[Byte], Array[Byte]) = {
      val entry = it.next()
      (entry.getKey, entry.getValue)
    }

    def nextKey(): Array[Byte] = it.next().getKey

    def close(): Unit = it.close()

  }

  protected def allKeys: DBPrefixIterator = {
    val it: DBIterator = db.iterator()
    new DBPrefixIterator(it, None)
  }

  def removeEverything(b: Option[WriteBatch]): Unit

  protected def makePrefix(prefix: Array[Byte]): Array[Byte] = Bytes.concat(prefix, Separator)

  protected def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(prefix, Separator, key, Separator)

  protected def makeKey(prefix: Array[Byte], key: String): Array[Byte] = makeKey(prefix, key.getBytes(Charset))

  protected def makeKey(prefix: Array[Byte], key: Int): Array[Byte] = makeKey(prefix, Ints.toByteArray(key))
}