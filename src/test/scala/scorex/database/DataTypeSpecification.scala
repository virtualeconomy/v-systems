package scorex.database

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}


class DataTypeSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert byte to DataType") {
    DataType(1) should be (DataType.ByteArray)
    DataType(2) should be (DataType.NoType)
  }

  property("convert DataType to byte") {
    DataType.ByteArray.id should be (1)
    DataType.NoType.id should be (2)
  }
}