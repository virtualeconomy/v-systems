package vsys.contract

import com.google.common.primitives.{Longs, Shorts, Chars}
import org.scalacheck.{Arbitrary, Gen}


trait DataStack {

  def initDataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    //shortText <- Gen.const(DataEntry(Shorts.toByteArray(desc.getBytes().length.toShort) ++ desc.getBytes(), DataType.ShortText))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

  def issueDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(max)
}

object DataStack {
  object initInput {
    val maxIndex: Byte = 0
    val unityIndex: Byte = 1
    val shortTextIndex: Byte = 2
    val issuerLoadIndex: Byte = 3
  }

  object issueInput {
    val amountIndex: Byte = 0
    val issuerGetIndex: Byte = 1
    val totalIndex: Byte = 2
  }

}
