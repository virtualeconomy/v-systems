package scorex.database

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError

class EntrySpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert entry to byte and convert back") {
    val name = "key1"
    val data = "value1"
    val entry = Entry.buildEntry(data, name, DataType.ByteArray)
    entry.map(_.bytes).map(_.arr).flatMap(Entry.fromBytes(_)) should be (entry)
  }

  property("report invalid data type") {
    val byteArray1 = Array[Byte](0, 4, 107, 101, 121, 49, 0, 118, 97, 108, 117, 101, 49)
    val byteArray2 = Array[Byte](0, 4, 107, 101, 121, 49, 3, 118, 97, 108, 117, 101, 49)
    val byteArray3 = Array[Byte](0, 4, 107, 101, 121, 49, 1, 118, 97, 108, 117, 101, 49)
    Entry.fromBytes(byteArray1) should be (Left(ValidationError.InvalidDataType))
    Entry.fromBytes(byteArray2) should be (Left(ValidationError.InvalidDataType))
    Entry.fromBytes(byteArray3).map(_.dataType) should be (Right(DataType.ByteArray))
  }

  property("report invalid data entry") {
    val byteArray1 = Array[Byte]()
    val byteArray2 = Array[Byte](1)
    val byteArray3 = Array[Byte](0, 4, 107, 101, 121, 49)
    val byteArray4 = Array[Byte](0, 4, 107, 101, 121)
    Entry.fromBytes(byteArray1) should be (Left(ValidationError.InvalidDataLength))
    Entry.fromBytes(byteArray2) should be (Left(ValidationError.InvalidDataLength))
    Entry.fromBytes(byteArray3) should be (Left(ValidationError.InvalidDataEntry))
    Entry.fromBytes(byteArray4) should be (Left(ValidationError.InvalidDataEntry))
  }
}