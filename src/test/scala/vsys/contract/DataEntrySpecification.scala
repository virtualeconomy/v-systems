package vsys.contract

import com.google.common.primitives.Longs
import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError.InvalidDataEntry

class DataEntrySpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  val validPublicKeyEntryGen: Gen[DataEntry] = for {
    data <- accountGen.map(_.publicKey)
  } yield DataEntry(data, DataType.PublicKey)

  val validAddressEntryGen: Gen[DataEntry] = for {
    data <- accountGen.map(PublicKeyAccount.toAddress(_)).map(_.bytes).map(_.arr)
  } yield DataEntry(data, DataType.Address)

  val validAmountEntryGen: Gen[DataEntry] = for {
    data <- positiveLongGen.map(Longs.toByteArray(_))
  } yield DataEntry(data, DataType.Amount)

  property("convert entry to bytes and convert back") {
    forAll(validPublicKeyEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validAddressEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validAmountEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }
  }

  property("report invalid data type") {
    val byteArray1 = Array[Byte](0, 118, 97, 108, 117, 101, 49)
    val byteArray2 = Array[Byte](4, 118, 97, 108, 117, 101, 49)
    val byteArray3 = Array.fill[Byte](1 + 32)(1)
    val byteArray4 = Array.fill[Byte](1 + 26)(2)
    val byteArray5 = Array.fill[Byte](1 + 8)(3)
    DataEntry.fromBytes(byteArray1) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray2) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray3).map(_.dataType) should be (Right(DataType.PublicKey))
    DataEntry.fromBytes(byteArray4).map(_.dataType) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray5).map(_.dataType) should be (Right(DataType.Amount))
  }

  private def assertEys(first: DataEntry, second: DataEntry): Unit = {
    first.bytes shouldEqual second.bytes
    first.data shouldEqual second.data
    first.dataType shouldEqual second.dataType
  }
}
