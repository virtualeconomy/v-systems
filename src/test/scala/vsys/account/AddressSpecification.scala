package vsys.account

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import vsys.utils.crypto.hash.SecureCryptographicHash._

class AddressSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("Address should be invalid for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash = hash(data).take(Address.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.value.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ hash(withoutChecksum).take(Address.ChecksumLength))
      Address.fromString(addressVersion2).isRight shouldBe (AddressVersion2 == Address.AddressVersion)
    }
  }

  property("Address should be invalid for incorrect chainId") {
    forAll { (data: Array[Byte], network: Byte) =>
      val withoutChecksum = Address.AddressVersion +: network +: hash(data).take(Address.HashLength)
      val addressBytes = withoutChecksum ++ hash(withoutChecksum).take(Address.ChecksumLength)
      Address.fromBytes(addressBytes).isRight shouldBe (network == AddressScheme.current.value.chainId)
    }
  }

  property("Address should be invalid for incorrect addressByteLength") {
    forAll { (data: Array[Byte], random: Int) =>
      whenever(random > -5 && random < 5) {
        val withoutChecksum = Address.AddressVersion +: AddressScheme.current.value.chainId +: hash(data).take(Address.HashLength + random)
        val addressBytes = withoutChecksum ++ hash(withoutChecksum).take(Address.ChecksumLength)
        Address.fromBytes(addressBytes).isRight shouldBe (random == 0)
      }
    }
  }
}
