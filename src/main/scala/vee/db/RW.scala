package vee.db

import org.iq80.leveldb.{DB, ReadOptions, WriteBatch}

class RW(db: DB, readOptions: ReadOptions, batch: WriteBatch) extends ReadOnlyDB(db, readOptions) {
  def put[V](key: Key[V], value: V): Unit = {
    val bytes = key.encode(value)
    batch.put(key.keyBytes, bytes)
  }

  def update[V](key: Key[V])(f: V => V): Unit = put(key, f(get(key)))

  def delete(key: Array[Byte], statsKey: String): Unit = batch.delete(key)

  def delete[V](key: Key[V]): Unit = batch.delete(key.keyBytes)

  def filterHistory(key: Key[Seq[Int]], heightToRemove: Int): Unit = put(key, get(key).filterNot(_ == heightToRemove))
}
