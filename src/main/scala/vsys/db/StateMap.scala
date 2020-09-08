package vsys.db

import java.nio.ByteBuffer

import org.h2.mvstore.`type`.{DataType, ObjectDataType}
import org.h2.mvstore.WriteBuffer
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}

import scala.collection.immutable.Stream

class StateMap[K, V](
  db: DB,
  stateName: String,
  keyType: DataType = new ObjectDataType,
  valueType: DataType = new ObjectDataType) extends SubStorage(db, "states") {


  private val StateNameBytes: Array[Byte] = stateName.getBytes(Charset)
  private val SizeBytes: Array[Byte] = "__size__".getBytes(Charset)
  private val Prefix: Array[Byte] = makePrefix(StateNameBytes)
  private val SizeKey: Array[Byte] = makeKey(StateNameBytes, SizeBytes)

  private def bytesOfKey(key: K): Array[Byte] = makeKey(StateNameBytes, getItemBytes(key, keyType))

  private def bytesOfValue(value: V): Array[Byte] = getItemBytes(value, valueType)

  private def getItemBytes(i: scala.Any, iType: DataType): Array[Byte] = {

    val iByteBuffer: WriteBuffer = new WriteBuffer(iType.getMemory(i))
    iType.write(iByteBuffer, i)
    iByteBuffer.getBuffer().array()

  }

  def put(key: K, value: V, batchOpt: Option[WriteBatch] = None): V = {
    invokeBatch(batchOpt, batch => {
      if (!containsKey(key)) setSize(sizeAsLong() + 1, batch)
      put(bytesOfKey(key), bytesOfValue(value), batch)
    })
    value
  }

  def get(key: K): Option[V] = {

    val valueBytesOption: Option[Array[Byte]] = get(bytesOfKey(key))
    if (valueBytesOption.isEmpty) None
    else Option(deserializeValue(valueBytesOption.get))

  }

  def containsKey(key: K): Boolean = {
    ! get(key).isEmpty
  }

  def remove(key: K, batchOpt: Option[WriteBatch] = None): Option[V] = {
    val rtn: Option[V] = get(key)
    if (rtn.isDefined) {
      invokeBatch(batchOpt, batch => {
        delete(bytesOfKey(key), batch)
        setSize(sizeAsLong() - 1, batch)
      })
    }
    rtn
  }


  def getKeyType(): DataType = keyType

  def getValueType():DataType = valueType

  def size(): Int = {

    val size: Long = sizeAsLong()
    if (size > Integer.MAX_VALUE ) Integer.MAX_VALUE
    else size.toInt

  }


  def sizeAsLong(): Long = {
    val sizeBytes: Option[Array[Byte]] = get(SizeKey)
    if (sizeBytes.isEmpty) 0
    else ByteBuffer.wrap(sizeBytes.get).getLong
  }

  private def setSize(size: Long, batch: Option[WriteBatch]): Unit = {
    put(SizeKey, ByteBuffer.allocate(8).putLong(size).array(), batch)
  }

  private def deserializeKey(keyBytes: Array[Byte]): K = {
    keyType.read(ByteBuffer.wrap(keyBytes.slice(Prefix.length, keyBytes.length))).asInstanceOf[K]
  }

  private def deserializeValue(valBytes: Array[Byte]): V = {
    valueType.read(ByteBuffer.wrap(valBytes)).asInstanceOf[V]
  }

  private def invokeBatch(batchOpt: Option[WriteBatch], op: Option[WriteBatch] => Unit): Unit = {
    var batch = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    op(batch)
    if (batchOpt.isEmpty) commit(batch)
  }

  def isEmpty(): Boolean = size() == 0

  def clear(batchOpt: Option[WriteBatch] = None): Unit = {
    invokeBatch(batchOpt, batch => {
      val it = allKeys
      while(it.hasNext) {
        val key = it.nextKey()
        if (key.startsWith(Prefix)) delete(key, batch)
      }
      it.close()
      setSize(0, batch)
    })
  }

  def asScala(): Stream[(K, V)] = {

    val it = allKeys
    val rtn = new Stream.StreamBuilder[(K, V)]()
    while (it.hasNext) {
      val (key, value) = it.next()
      if (key.startsWith(Prefix) && !key.startsWith(SizeKey)) {
        rtn += ((deserializeKey(key), deserializeValue(value)): (K, V))
      }
    }
    it.close()
    rtn.result()

  }

  def rangeQuery(start: K, end: K): Stream[(K, V)]= {
    val it: DBRangeIterator = new DBRangeIterator(db.iterator(), Some(bytesOfKey(start)), Some(bytesOfKey(end)), true, false)
    val rtn = new Stream.StreamBuilder[(K, V)]()
    while (it.hasNext) {
      val (key, value) = it.next()
      if (!key.startsWith(SizeKey)) {
        rtn += ((deserializeKey(key), deserializeValue(value)): (K, V))
      }
    }
    it.close()
    rtn.result()
  }

  override def allKeys: DBPrefixIterator = {
    val it: DBIterator = db.iterator()
    new DBPrefixIterator(it, Some(Prefix))
  }

}