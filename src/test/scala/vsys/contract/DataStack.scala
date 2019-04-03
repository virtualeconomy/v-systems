package vsys.contract

import com.google.common.primitives.{Longs, Ints}
import org.scalacheck.Gen


trait DataStack {

  def initDataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    //shortText <- Gen.const(DataEntry(Shorts.toByteArray(desc.getBytes().length.toShort) ++ desc.getBytes(), DataType.ShortText))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

  def issueDataStackGen(amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(max, index)
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
    val tokenIndex: Byte = 1
    val issuerGetIndex: Byte = 2
  }

}
