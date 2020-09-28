package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.transaction.TransactionGen
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.account.PublicKeyAccount
import vsys.blockchain.transaction.ValidationError.InvalidDataEntry
import vsys.utils.serialization.Deser

class DataEntrySpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  val validPublicKeyEntryGen: Gen[DataEntry] = for {
    data <- accountGen.map(_.publicKey)
  } yield DataEntry(data, DataType.PublicKey)

  val validAddressEntryGen: Gen[DataEntry] = for {
    data <- accountGen.map(PublicKeyAccount.toAddress(_)).map(_.bytes).map(_.arr)
  } yield DataEntry(data, DataType.Address)

  val validAmountEntryGen: Gen[DataEntry] = for {
    data <- positiveLongGen.map(Longs.toByteArray)
  } yield DataEntry(data, DataType.Amount)

  val validInt32EntryGen: Gen[DataEntry] = for {
    data <- positiveIntGen.map(Ints.toByteArray)
  } yield DataEntry(data, DataType.Int32)

  val validShortTextEntryGen: Gen[DataEntry] = for {
    data <- attachmentGen.map(Deser.serializeArray)
  } yield DataEntry(data, DataType.ShortText)

  val validContractAccountEntryGen: Gen[DataEntry] = for {
    data <- contractAccountGen.map(_.bytes.arr)
  } yield DataEntry(data, DataType.ContractAccount)

  val validTimestampEntryGen: Gen[DataEntry] = for {
    data <- timestampGen.map(Longs.toByteArray)
  } yield DataEntry(data, DataType.Timestamp)

  val validBooleanEntryGen: Gen[DataEntry] = for {
    data <- byteArrayGen(1).map(_.deep == Array(1.toByte).deep).map(if(_) 1 else 0).map(_.toByte).map(Array(_))
  } yield DataEntry(data, DataType.Boolean)

  val validShortBytesEntryGen: Gen[DataEntry] = for {
    data <- attachmentGen.map(Deser.serializeArray)
  } yield DataEntry(data, DataType.ShortBytes)

  val validOpcBlockEntryGen: Gen[DataEntry] = for {
    data <- attachmentGen.map(Deser.serializeArray)
  } yield DataEntry(data, DataType.OpcBlock)

  val validBigIntegerEntryGen: Gen[DataEntry] = for {
    data <- attachmentGen.map(Deser.serializeArray)
  } yield DataEntry(data, DataType.BigInteger)

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

    forAll(validInt32EntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validShortTextEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validContractAccountEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validTimestampEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validBooleanEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validShortBytesEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validOpcBlockEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

    forAll(validBigIntegerEntryGen) { pk: DataEntry =>
      val recovered = DataEntry.fromBytes(pk.bytes).right.get

      assertEys(recovered, pk)
    }

  }

  property("report invalid and valid data type") {
    val byteArray1 = Array[Byte](0, 118, 97, 108, 117, 101, 49)
    val byteArray2 = Array[Byte](4, 118, 97, 108, 117, 101, 49)
    val byteArray3 = Array.fill[Byte](1 + 32)(1)
    val byteArray4 = Array.fill[Byte](1 + 26)(2)
    val byteArray5 = Array.fill[Byte](1 + 8)(3)
    val byteArray6 = Array.fill[Byte](1 + 4)(4)
    val byteArray7 = Array[Byte](5, 0, 0)
    val byteArray8 = Array.fill[Byte](1 + 26)(6)
    val byteArray9 = Array.fill[Byte](1 + 26)(7)
    val byteArray10 = Array.fill[Byte](1 + 8)(9)
    val byteArray11 = Array[Byte](10, 1)
    val byteArray12 = Array[Byte](11, 0, 0)
    val byteArray13 = Array[Byte](12, 118, 97, 108, 117, 101, 49)
    val byteArray14 = Array[Byte](13, 0, 0)
    val byteArray15 = Array[Byte](14, 0, 0)
    DataEntry.fromBytes(byteArray1) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray2) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray3).map(_.dataType) should be (Right(DataType.PublicKey))
    DataEntry.fromBytes(byteArray4).map(_.dataType) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray5).map(_.dataType) should be (Right(DataType.Amount))
    DataEntry.fromBytes(byteArray6).map(_.dataType) should be (Right(DataType.Int32))
    DataEntry.fromBytes(byteArray7).map(_.dataType) should be (Right(DataType.ShortText))
    DataEntry.fromBytes(byteArray8).map(_.dataType) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray9).map(_.dataType) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray10).map(_.dataType) should be (Right(DataType.Timestamp))
    DataEntry.fromBytes(byteArray11).map(_.dataType) should be (Right(DataType.Boolean))
    DataEntry.fromBytes(byteArray12).map(_.dataType) should be (Right(DataType.ShortBytes))
    DataEntry.fromBytes(byteArray13).map(_.dataType) should be (Left(InvalidDataEntry))
    DataEntry.fromBytes(byteArray14).map(_.dataType) should be (Right(DataType.OpcBlock))
    DataEntry.fromBytes(byteArray15).map(_.dataType) should be (Right(DataType.BigInteger))
  }

  private def assertEys(first: DataEntry, second: DataEntry): Unit = {
    first.bytes shouldEqual second.bytes
    first.data shouldEqual second.data
    first.dataType shouldEqual second.dataType
  }
}
