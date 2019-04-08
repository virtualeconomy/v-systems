package vsys.contract

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import scorex.account.Address


trait DataStack {

  def initDataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

}

