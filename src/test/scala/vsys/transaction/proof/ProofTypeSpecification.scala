package vsys.transaction.proof

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}


class ProofTypeSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("convert byte to ProofType") {
    ProofType.fromByte(1) should be (Some(ProofType.Curve25519))
    ProofType.fromByte(2) should be (None)
    ProofType.fromByte(0) should be (None)
    ProofType.fromByte(3) should be (None)
  }

  property("convert ProofType to byte") {
    ProofType.Curve25519.id should be (1)
  }
}