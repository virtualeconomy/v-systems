/*
 * Copyright 2004-2019 H2 Group. Multiple-Licensed under the MPL 2.0,
 * and the EPL 1.0 (http://h2database.com/html/license.html).
 * Initial Developer: H2 Group
 */
package vsys.db

import java.nio.ByteBuffer

import org.h2.mvstore.`type`.DataType
import org.h2.mvstore.`type`.ObjectDataType
import org.h2.mvstore.WriteBuffer
import org.iq80.leveldb.DB


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

  def put(key: K, value: V): V = {

    val Array(keyBytes, valBytes) = 
      Array((key, keyType), (value, valueType)).map {case(i, iType) => getItemBytes(i, iType)}
    put(makeKey(StateNameBytes, keyBytes), valBytes, None)
    setSize(sizeAsLong() + 1)
    value

  }

  def get(key: K): Option[V] = {

    val valueBytesOption: Option[Array[Byte]] = get(makeKey(StateNameBytes, getItemBytes(key, keyType)))
    if (valueBytesOption.isEmpty) None
    else Option(valueType.read(ByteBuffer.wrap(valueBytesOption.get)).asInstanceOf[V])

  }

  def containsKey(key: K): Boolean = {
    ! get(key).isEmpty
  }

  def remove(key: K): Option[V] = {
    val rtn: Option[V] = get(key)
    if (!rtn.isEmpty) {
      delete(makeKey(StateNameBytes, getItemBytes(key, keyType)), None)
      setSize(sizeAsLong() - 1)
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

  private def setSize(size: Long): Unit = {

    put(SizeKey, ByteBuffer.allocate(8).putLong(size).array(), None)
      
  }

  def isEmpty(): Boolean = size() == 0

  def clear(): Unit = {

    val it = allKeys
    while(it.hasNext) {
      val key = it.next()
      if (key.startsWith(Prefix)) delete(key, None)
    }
    it.close()
    setSize(0)

  }

  def asScala(): Array[(K, V)] = {

    val it = allKeys
    var rtn = Array[(K, V)]()
    while (it.hasNext) {
      val key = it.next()
      if (key.startsWith(Prefix) && !key.startsWith(SizeKey)) {
        val kk: K = keyType.read(ByteBuffer.wrap(key)).asInstanceOf[K]
        rtn :+= (kk, get(kk).get)
      }
    }
    it.close()
    rtn

  }

}