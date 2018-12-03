package vsys.database

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}


class DataTypeSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert byte to DataType") {
    DataType.fromByte(1) should be (Some(DataType.ByteArray))
    DataType.fromByte(2) should be (None)
    DataType.fromByte(0) should be (None)
    DataType.fromByte(3) should be (None)
  }

  property("convert DataType to byte") {
    DataType.ByteArray.id should be (1)
  }
}