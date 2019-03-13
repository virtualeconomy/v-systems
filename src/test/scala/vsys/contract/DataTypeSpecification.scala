package vsys.contract

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class DataTypeSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("convert byte to DataType") {
    DataType.fromByte(1) should be (Some(DataType.PublicKeyAccount))
    DataType.fromByte(2) should be (Some(DataType.Address))
    DataType.fromByte(3) should be (Some(DataType.Amount))
    DataType.fromByte(4) should be (Some(DataType.Index))
    DataType.fromByte(5) should be (Some(DataType.Description))
    DataType.fromByte(0) should be (None)
    DataType.fromByte(6) should be (None)
  }

  property("convert DataType to byte") {
    DataType.PublicKeyAccount.id should be (1)
    DataType.Address.id should be (2)
    DataType.Amount.id should be (3)
    DataType.Index.id should be (4)
    DataType.Description.id should be (5)
  }
}
