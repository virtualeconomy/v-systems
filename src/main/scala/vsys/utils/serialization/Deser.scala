package vsys.utils.serialization

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder}

import com.google.common.primitives.{Bytes, Ints, Shorts}

object Deser {

  def serializeArray(b: Array[Byte]): Array[Byte] = Shorts.toByteArray(b.length.toShort) ++ b

  def serializeArrays(bs: Seq[Array[Byte]]): Array[Byte] = Shorts.toByteArray(bs.length.toShort) ++ Bytes.concat(bs.map(serializeArray): _*)

  def parseArraySize(bytes: Array[Byte], position: Int): (Array[Byte], Int) = {
    val length = Shorts.fromByteArray(bytes.slice(position, position + 2))
    (bytes.slice(position + 2, position + 2 + length), position + 2 + length)
  }

  def parseOption(bytes: Array[Byte], position: Int, length: Int): (Option[Array[Byte]], Int) = {
    bytes.slice(position, position + 1).headOption match {
      case Some(1): Option[Byte] => {
        val b = bytes.slice(position + 1, position + 1 + length)
        (Some(b), position + 1 + length)
      }
      case _ => (None, position + 1)
    }
  }

  def parseArrays(bytes: Array[Byte]): Seq[Array[Byte]] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[Array[Byte]], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArraySize(bytes, pos)
        (acc :+ arr, nextPos)
    }
    r match { case (parsedArray, _) => parsedArray}
  }

  val encoder = ThreadLocal.withInitial[CharsetEncoder](() => Charset.forName("UTF-8").newEncoder);
  def decoder = ThreadLocal.withInitial[CharsetDecoder](() => Charset.forName("UTF-8").newDecoder);

  def validUTF8(string: String): Boolean = {
    encoder.get().canEncode(string)
  }

  def serilizeString(string: String) : Array[Byte] = {
    val bytes: ByteBuffer = encoder.get().encode(CharBuffer.wrap(string))
    bytes.array.slice(bytes.position, bytes.limit)
  }

  def deserilizeString(bytes: Array[Byte]) :String = {
    decoder.get().decode(ByteBuffer.wrap(bytes)).toString
  }

  def serializeInts(is: Seq[Int]): Array[Byte] = {
    Shorts.toByteArray(is.length.toShort) ++ Bytes.concat(is.map(Ints.toByteArray): _*)
  }

  def deserializeInts(bytes: Array[Byte]): Seq[Int] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[Int], 2)) {
      case ((acc, pos), _) =>
        val arr = Ints.fromByteArray(bytes.slice(pos, pos + 4))
        (acc :+ arr, pos + 4)
    }
    r match { case (parsedInts, _) => parsedInts}
  }

  def serializeShorts(is: Seq[Short]): Array[Byte] = {
    Shorts.toByteArray(is.length.toShort) ++ Bytes.concat(is.map(Shorts.toByteArray): _*)
  }

  def deserializeShorts(bytes: Array[Byte]): Seq[Short] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[Short], 2)) {
      case ((acc, pos), _) =>
        val arr = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
        (acc :+ arr, pos + 2)
    }
    r match { case (parsedShorts, _) => parsedShorts}
  }
}
