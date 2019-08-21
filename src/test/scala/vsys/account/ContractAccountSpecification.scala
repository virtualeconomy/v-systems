package vsys.account

import com.google.common.primitives.Ints
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import vsys.utils.crypto.hash.SecureCryptographicHash._

class ContractAccountSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("Contract address should be invalid for another address version") {
    forAll { (data: Array[Byte], AddressVersion2: Byte) =>
      val publicKeyHash = hash(data).take(ContractAccount.HashLength)
      val withoutChecksum = AddressVersion2 +: AddressScheme.current.chainId +: publicKeyHash
      val addressVersion2 = Base58.encode(withoutChecksum ++ hash(withoutChecksum).take(ContractAccount.ChecksumLength))
      ContractAccount.fromString(addressVersion2).isRight shouldBe (AddressVersion2 == ContractAccount.AddressVersion)
    }
  }

  property("Contract address should be invalid for incorrect chainId") {
    forAll { (data: Array[Byte], network: Byte) =>
      val withoutChecksum = ContractAccount.AddressVersion +: network +: hash(data).take(ContractAccount.HashLength)
      val addressBytes = withoutChecksum ++ hash(withoutChecksum).take(ContractAccount.ChecksumLength)
      ContractAccount.fromBytes(addressBytes).isRight shouldBe (network == AddressScheme.current.chainId)
    }
  }

  property("Contract address should be invalid for incorrect addressByteLength") {
    forAll { (data: Array[Byte], random: Int) =>
      whenever(random > -5 && random < 5) {
        val withoutChecksum = ContractAccount.AddressVersion +: AddressScheme.current.chainId +: hash(data).take(Address.HashLength + random)
        val addressBytes = withoutChecksum ++ hash(withoutChecksum).take(Address.ChecksumLength)
        ContractAccount.fromBytes(addressBytes).isRight shouldBe (random == 0)
        ContractAccount.tokenIdFromBytes(addressBytes, Ints.toByteArray(0)).isRight shouldBe (random == 0)
      }
    }
  }

  property("Contract Id and Token Id should be consistent") {
    forAll { (data: Array[Byte]) =>
      val withoutChecksum = ContractAccount.AddressVersion +: AddressScheme.current.chainId +: hash(data).take(ContractAccount.HashLength)
      val addressBytes = withoutChecksum ++ hash(withoutChecksum).take(ContractAccount.ChecksumLength)
      val idxBytes = Ints.toByteArray(0)
      val tokenId = ContractAccount.tokenIdFromBytes(addressBytes, idxBytes).right.get
      ContractAccount.fromBytes(addressBytes).right.get.bytes shouldEqual ContractAccount.contractIdFromBytes(tokenId.arr)
    }
  }

  property("From String with and without prefix should be consistent") {
    forAll { data: Array[Byte] =>
      val publicKeyHash = hash(data).take(ContractAccount.HashLength)
      val withoutChecksum = ContractAccount.AddressVersion +: AddressScheme.current.chainId +: publicKeyHash
      val bytes = withoutChecksum ++ hash(withoutChecksum).take(ContractAccount.ChecksumLength)
      val addressStr1 = Base58.encode(bytes)
      val addressStr2 = "contractAccount:" + addressStr1
      ContractAccount.fromString(addressStr1) shouldEqual ContractAccount.fromBytes(bytes)
      ContractAccount.fromString(addressStr1) shouldEqual ContractAccount.fromString(addressStr2)
      ContractAccount.fromString(addressStr1).right.get should not equal None
      ContractAccount.fromString("1" * (ContractAccount.AddressStringLength + 1)).isRight shouldBe false
    }
  }
}
