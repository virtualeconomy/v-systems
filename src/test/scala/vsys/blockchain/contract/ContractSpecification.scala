package vsys.blockchain.contract

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class ContractSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {
  property("contract structure is at least 8 bytes") {
    Contract.LanguageCodeByteLength should be (4)
    Contract.LanguageVersionByteLength should be (4)
    Contract.MinContractByteSize should be (8)
  }
}
