package vsys.blockchain.contract

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class DataTypeSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("convert byte to DataType") {
    DataType.fromByte(1) should be (Some(DataType.PublicKey))
    DataType.fromByte(2) should be (Some(DataType.Address))
    DataType.fromByte(3) should be (Some(DataType.Amount))
    DataType.fromByte(4) should be (Some(DataType.Int32))
    DataType.fromByte(5) should be (Some(DataType.ShortText))
    DataType.fromByte(6) should be (Some(DataType.ContractAccount))
    DataType.fromByte(7) should be (Some(DataType.Account))
    DataType.fromByte(8) should be (Some(DataType.TokenId))
    DataType.fromByte(0) should be (None)
    DataType.fromByte(9) should be (None)
  }

  property("convert DataType to byte") {
    DataType.PublicKey.id should be (1)
    DataType.Address.id should be (2)
    DataType.Amount.id should be (3)
    DataType.Int32.id should be (4)
    DataType.ShortText.id should be (5)
    DataType.ContractAccount.id should be (6)
    DataType.Account.id should be (7)
    DataType.TokenId.id should be (8)
  }
}
