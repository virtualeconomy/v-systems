package vee.database

import org.scalacheck.Gen
import org.scalacheck.Gen.{alphaNumChar, frequency}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError

class EntrySpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  val longStringGen: Gen[String] = for {
    randomCharSet <- frequency[Char]((1, alphaNumChar))
    aliasChars <- Gen.listOfN(20000, randomCharSet)
  } yield aliasChars.mkString

  val invalidEntryGen: Gen[Either[ValidationError, Entry]] = for {
    data: String <- longStringGen
  } yield Entry.buildEntry(data, DataType.ByteArray)

  property("convert entry to byte and convert back") {
    val data = "value1"
    val entry = Entry.buildEntry(data, DataType.ByteArray)
    entry.map(_.bytes).map(_.arr).flatMap(Entry.fromBytes(_)) should be (entry)
  }

  property("report invalid data type") {
    val byteArray1 = Array[Byte](0, 118, 97, 108, 117, 101, 49)
    val byteArray2 = Array[Byte](3, 118, 97, 108, 117, 101, 49)
    val byteArray3 = Array[Byte](1, 118, 97, 108, 117, 101, 49)
    Entry.fromBytes(byteArray1) should be (Left(ValidationError.InvalidDataType))
    Entry.fromBytes(byteArray2) should be (Left(ValidationError.InvalidDataType))
    Entry.fromBytes(byteArray3).map(_.dataType) should be (Right(DataType.ByteArray))
  }

  property("report invalid data length") {
    val byteArray1 = Array[Byte]()
    Entry.fromBytes(byteArray1) should be (Left(ValidationError.InvalidDataLength))
  }

  property("report exceeds data length") {
    forAll(invalidEntryGen) { entry: Either[ValidationError, Entry] =>
      entry shouldEqual Left(ValidationError.TooLongDbEntry(20000, Entry.maxLength))
    }
  }

}