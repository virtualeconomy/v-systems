package vsys.account

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.encode.Base58
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.TransactionGen

class PublicKeyAccountSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("Invalid base58 string should produce ValidationError") {
    forAll { s: String =>
      whenever(s.length <= TransactionParser.KeyStringLength) {
        PublicKeyAccount.fromBase58String(s).isRight shouldBe Base58.decode(s).toEither.isRight
      }
    }
  }

  property("Equals returns false when type mismatching") {
    forAll(accountGen) { account: PublicKeyAccount =>
      account.equals(PublicKeyAccount(account.publicKey)) shouldBe true
      account.equals(account.publicKey) shouldBe false
    }
  }
}
