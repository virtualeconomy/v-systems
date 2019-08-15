package vsys.blockchain.state

import java.nio.ByteBuffer

import org.h2.mvstore.DataUtils._
import org.h2.mvstore.WriteBuffer
import org.h2.mvstore.`type`.DataType
import scorex.utils.ByteArray

object DataTypes {
  trait DTTemplate extends DataType {
    override def read(buff: ByteBuffer, obj: Array[AnyRef], len: Int, key: Boolean) =
      0 until len foreach { i => obj(i) = read(buff) }

    override def write(buff: WriteBuffer, obj: Array[AnyRef], len: Int, key: Boolean) =
      0 until len foreach { i => write(buff, obj(i)) }
  }

  val byteStr: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any): Int = ByteArray.compare(a.asInstanceOf[ByteStr].arr, b.asInstanceOf[ByteStr].arr)

    override def read(buff: ByteBuffer): AnyRef = {
      val len = readVarInt(buff)
      val dst = Array.fill(len)(0: Byte)
      buff.get(dst, 0, len)
      ByteStr(dst)
    }

    override def getMemory(obj: scala.Any): Int = 5 + obj.asInstanceOf[ByteStr].arr.length

    override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
      val byteStr = obj.asInstanceOf[ByteStr]
      buff.putVarInt(byteStr.arr.length).put(byteStr.arr)
    }
  }

  // (Boolean, Long)
  val assets: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Boolean, Long)]].compare(a.asInstanceOf[(Boolean, Long)], b.asInstanceOf[(Boolean, Long)])

    override def read(buff: ByteBuffer) = (buff.get() == 1, readVarLong(buff))

    override def getMemory(obj: scala.Any) = 10

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (b, l) = obj.asInstanceOf[(Boolean, Long)]
      buff.put(if (b) 1.toByte else 0.toByte)
      buff.putVarLong(l)
    }
  }

  // (Long, Long)
  val orderFills: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Long, Long)]].compare(a.asInstanceOf[(Long, Long)], b.asInstanceOf[(Long, Long)])

    override def read(buff: ByteBuffer) = (readVarLong(buff), readVarLong(buff))

    override def getMemory(obj: scala.Any) = 18

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (l1, l2) = obj.asInstanceOf[(Long, Long)]
      buff.putVarLong(l1)
      buff.putVarLong(l2)
    }
  }

  val portfolios: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val v1 = readVarLong(buff)
      val v2 = readVarLong(buff)
      val v3 = readVarLong(buff)
      val m = Seq.fill(readVarInt(buff)) {
        val length = readVarInt(buff)
        val bytes = new Array[Byte](length)
        buff.get(bytes)
        (bytes, readVarLong(buff))
      }.toMap

      (v1, (v2, v3), m)
    }

    override def getMemory(obj: scala.Any) = {
      val (_, _, m) = obj.asInstanceOf[(Long, (Long, Long), Map[Array[Byte], Long])]
      9 * 3 + 5 + m.keySet.map(_.length + 5 + 9).sum
    }

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (v1, (v2, v3), m) = obj.asInstanceOf[(Long, (Long, Long), Map[Array[Byte], Long])]
      buff.putVarLong(v1)
        .putVarLong(v2)
        .putVarLong(v3)
        .putVarInt(m.size)

      for ((k, v) <- m) {
        buff.putVarInt(k.length)
          .put(k)
          .putVarLong(v)
      }
    }
  }

  // (Int, Long, Long, Long)
  val balanceSnapshots: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Int, Long, Long, Long)]].compare(a.asInstanceOf[(Int, Long, Long, Long)], b.asInstanceOf[(Int, Long, Long, Long)])

    override def read(buff: ByteBuffer) = (readVarInt(buff), readVarLong(buff), readVarLong(buff), readVarLong(buff))

    // Long = 9, Int = 5
    override def getMemory(obj: scala.Any) = 32

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (v1, v2, v3, v4) = obj.asInstanceOf[(Int, Long, Long, Long)]
      buff.putVarInt(v1)
      buff.putVarLong(v2)
      buff.putVarLong(v3)
      buff.putVarLong(v4)
    }
  }

  // (Int, Array[Byte])
  val transactions: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val i = readVarInt(buff)
      val b = new Array[Byte](readVarInt(buff))
      buff.get(b)
      (i, b)
    }

    override def getMemory(obj: scala.Any) = 10 + obj.asInstanceOf[(Int, Array[Byte])]._2.length

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (i, v) = obj.asInstanceOf[(Int, Array[Byte])]
      buff.putVarInt(i)
      buff.putVarInt(v.length)
      buff.put(v)
    }
  }

  // (Int, ByteStr, Array[Byte])
  val contracts: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer): AnyRef = {
      val i = readVarInt(buff)
      val dst = new Array[Byte](readVarInt(buff))
      buff.get(dst)
      val b = new Array[Byte](readVarInt(buff))
      buff.get(b)
      (i, ByteStr(dst), b)
    }

    override def getMemory(obj: scala.Any) = {
      val (_, bstr, ba) = obj.asInstanceOf[(Int, ByteStr, Array[Byte])]
      10 + bstr.arr.length + 5 + ba.length
    }

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (i, bstr, ba) = obj.asInstanceOf[(Int, ByteStr, Array[Byte])]
      buff.putVarInt(i)
        .putVarInt(bstr.arr.length)
        .put(bstr.arr)
        .putVarInt(ba.length)
        .put(ba)
    }
  }
}
