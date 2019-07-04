package vsys.db

import com.google.common.primitives.{Bytes, Ints}
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}

class SubStorage(db: DB, name: String) extends Storage(db) {

  private val subPrefix: Array[Byte] = name.getBytes(Charset)

  override protected def makePrefix(prefix: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator)

  override protected def makeKey(prefix: Array[Byte], key: Array[Byte]): Array[Byte] = Bytes.concat(subPrefix, Separator, prefix, Separator, key)

  override protected def makeKey(prefix: Array[Byte], key: String): Array[Byte] = makeKey(prefix, key.getBytes(Charset))

  override protected def makeKey(prefix: Array[Byte], key: Int): Array[Byte] = makeKey(prefix, Ints.toByteArray(key))

  override def removeEverything(b: Option[WriteBatch]): Unit = {
    val it = allKeys
    while (it.hasNext) {
      val key = it.nextKey()
      if (key.startsWith(subPrefix)) delete(key, b)
    }
    it.close()
  }

  override protected def allKeys: DBPrefixIterator = {
    val it: DBIterator = db.iterator()
    new DBPrefixIterator(it, Some(subPrefix))
  }

}