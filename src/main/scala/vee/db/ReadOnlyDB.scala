package vee.db

import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

class ReadOnlyDB(db: DB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(key.keyBytes, readOptions)
    key.parse(bytes)
  }

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.keyBytes, readOptions)
    bytes != null
  }

  def iterator: DBIterator = db.iterator(readOptions)
}
