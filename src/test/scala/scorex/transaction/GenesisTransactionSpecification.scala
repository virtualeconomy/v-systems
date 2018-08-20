package scorex.transaction

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.TransactionType

import scala.util.{Failure, Try}

class GenesisTransactionSpecification extends PropSpec with PropertyChecks with Matchers {

  private val defaultRecipient = PublicKeyAccount(Array.fill(32)(0: Byte))


  def parseBytes(data: Array[Byte]): Try[GenesisTransaction] = {
    data.head match {
      case transactionType: Byte if transactionType == TransactionType.GenesisTransaction.id =>
        GenesisTransaction.parseTail(data.tail)
      case transactionType =>
        Failure(new Exception(s"Incorrect transaction type '$transactionType' in GenesisTransaction data"))
    }
  }

  property("GenesisTransaction Signature should be the same") {
    val balance = 457L
    val timestamp = 2398762345L
    val signature = GenesisTransaction.generateSignature(defaultRecipient, balance, -1, timestamp)

    val expected = "Hv3aQeyuKNkTHcJZsgUyZmcoc8U4UUDB4KXMQ2j44stoKxq1RhLFNyLXACvE3q16udwQHBq5XRcuDaDRcsNn4Y9"
    val actual = Base58.encode(signature)

    assert(actual == expected)
  }


  property("GenesisTransaction parse from Bytes should work fine") {
    val bytes = Base58.decode("UxYztEXtoSnQZ3krg7LNoGGvQCHS1NnJmGLRARbKTLLz3M63XZ3zxn4pjKvM4XQ").get

    val actualTransaction = parseBytes(bytes).get

    val balance = 12345L
    val timestamp = 1234567890L
    val expectedTransaction = GenesisTransaction.create(defaultRecipient, balance, -1, timestamp).right.get

    actualTransaction should equal(expectedTransaction)
  }

  property("GenesisTransaction serialize/deserialize roundtrip") {
    forAll(Gen.listOfN(32, Arbitrary.arbitrary[Byte]).map(_.toArray), Gen.posNum[Long], Gen.posNum[Long]) {
      (recipientSeed: Array[Byte], time: Long, amount: Long) =>
        val recipient = PrivateKeyAccount(recipientSeed)
        val source = GenesisTransaction.create(recipient, amount, -1, time).right.get
        val bytes = source.bytes
        val dest = parseBytes(bytes).get

        source should equal(dest)
    }
  }

}
