package vee.database

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError

class EntrySpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

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
}