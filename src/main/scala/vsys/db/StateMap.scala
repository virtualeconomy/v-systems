package vsys.db

import java.nio.ByteBuffer
import scala.collection.immutable.Stream

import org.h2.mvstore.`type`.DataType
import org.h2.mvstore.`type`.ObjectDataType
import org.h2.mvstore.WriteBuffer
import org.iq80.leveldb.{DB, DBIterator, WriteBatch}


class StateMap[K, V](
  db: DB,
  stateName: String,
  keyType: DataType = new ObjectDataType,
  valueType: DataType = new ObjectDataType) extends SubStorage(db, "states") {


  private val StateNameBytes: Array[Byte] = stateName.getBytes(Charset)
  private val SizeBytes: Array[Byte] = "__size__".getBytes(Charset)
  private val Prefix: Array[Byte] = makePrefix(StateNameBytes)
  private val SizeKey: Array[Byte] = makeKey(StateNameBytes, SizeBytes)

  private def getItemBytes(i: scala.Any, iType: DataType): Array[Byte] = {

    val iByteBuffer: WriteBuffer = new WriteBuffer(iType.getMemory(i))
    iType.write(iByteBuffer, i)
    iByteBuffer.getBuffer().array()

  }

  def put(key: K, value: V, batchOpt: Option[WriteBatch] = None): V = {
    var batch: Option[WriteBatch] = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    val Array(keyBytes, valBytes) = 
      Array((key, keyType), (value, valueType)).map {case(i, iType) => getItemBytes(i, iType)}
    if (!containsKey(key)) setSize(sizeAsLong() + 1, batch)
    put(makeKey(StateNameBytes, keyBytes), valBytes, batch)
    if (batchOpt.isEmpty) commit(batch)
    value

  }

  def get(key: K): Option[V] = {

    val valueBytesOption: Option[Array[Byte]] = get(makeKey(StateNameBytes, getItemBytes(key, keyType)))
    if (valueBytesOption.isEmpty) None
    else Option(deserializeValue(valueBytesOption.get))

  }

  def containsKey(key: K): Boolean = {
    ! get(key).isEmpty
  }

  def remove(key: K, batchOpt: Option[WriteBatch] = None): Option[V] = {
    val rtn: Option[V] = get(key)
    if (rtn.isDefined) {
      var batch: Option[WriteBatch] = batchOpt
      if (batchOpt.isEmpty) batch = createBatch()
      delete(makeKey(StateNameBytes, getItemBytes(key, keyType)), batch)
      setSize(sizeAsLong() - 1, batch)
      if (batchOpt.isEmpty) commit(batch)
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

  def isEmpty(): Boolean = size() == 0

  def clear(batchOpt: Option[WriteBatch] = None): Unit = {
    var batch: Option[WriteBatch] = batchOpt
    if (batchOpt.isEmpty) batch = createBatch()
    val it = allKeys
    while(it.hasNext) {
      val key = it.nextKey()
      if (key.startsWith(Prefix)) delete(key, batch)
    }
    it.close()
    setSize(0, batch)
    if (batchOpt.isEmpty) commit(batch)

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

  override def allKeys: DBPrefixIterator = {
    val it: DBIterator = db.iterator()
    new DBPrefixIterator(it, Some(Prefix))
  }

}