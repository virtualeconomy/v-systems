package vsys.contract

import com.google.common.primitives.Longs
import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError

class DataEntrySpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  val validPublicKeyEntryGen: Gen[DataEntry] = for {
    data <- accountGen.map(_.publicKey)
  } yield DataEntry(data, DataType.PublicKeyAccount)

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
    val byteArray6 = Array[Byte](1, 118, 97, 108, 117, 101, 49)
    val byteArray7 = Array[Byte](2, 118, 97, 108, 117, 101, 49)
    val byteArray8 = Array[Byte](3, 118, 97, 108, 117, 101, 49)
    val byteArray9 = byteArray3 ++ byteArray4 ++ byteArray5
    DataEntry.fromBytes(byteArray1) should be (Left(ValidationError.InvalidDataType))
    DataEntry.fromBytes(byteArray2) should be (Left(ValidationError.InvalidDataType))
    DataEntry.fromBytes(byteArray3).map(_.dataType) should be (Right(DataType.PublicKeyAccount))
    DataEntry.fromBytes(byteArray4).map(_.dataType) should be (Right(DataType.Address))
    DataEntry.fromBytes(byteArray5).map(_.dataType) should be (Right(DataType.Amount))
    DataEntry.fromArrayBytes(byteArray6) should be (Left(ValidationError.InvalidDataLength))
    DataEntry.fromArrayBytes(byteArray7) should be (Left(ValidationError.InvalidDataLength))
    DataEntry.fromArrayBytes(byteArray8) should be (Left(ValidationError.InvalidDataLength))
    DataEntry.fromArrayBytes(byteArray9).map(_.head.dataType) should be (Right(DataType.PublicKeyAccount))
    DataEntry.fromArrayBytes(byteArray9).map(_.last.dataType) should be (Right(DataType.Amount))
    DataEntry.fromArrayBytes(byteArray9).map(_(1).dataType) should be (Right(DataType.Address))
  }

  private def assertEys(first: DataEntry, second: DataEntry): Unit = {
    first.bytes shouldEqual second.bytes
    first.data shouldEqual second.data
    first.dataType shouldEqual second.dataType
  }
}
