package vsys.contract

import com.google.common.primitives.Shorts
import org.scalacheck.{Arbitrary, Gen}

trait ByteArrayGen {
  private def byteArrayGen(length: Short): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  private val minSize: Short = 1
  private val maxSize: Short = 127

  def byteArrayRandomGen(): Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minSize, maxSize)
    bytes <- byteArrayGen(length)
  } yield Shorts.toByteArray(length) ++ bytes
}