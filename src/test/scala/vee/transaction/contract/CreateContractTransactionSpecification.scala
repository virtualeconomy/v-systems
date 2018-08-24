package vee.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._

class CreateContractTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("CreateContractTransaction serialization roundtrip") {
    forAll(createContractGen) { tx: CreateContractTransaction =>
      require(tx.bytes.head == TransactionType.CreateContractTransaction.id)
      val recovered = CreateContractTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("CreateContractTransaction serialization from TypedTransaction") {
    forAll(createContractGen) { tx: CreateContractTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[CreateContractTransaction], tx)
    }
  }

  private def assertTxs(first: CreateContractTransaction, second: CreateContractTransaction): Unit = {
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.contract.name shouldEqual second.contract.name
    first.contract.content shouldEqual second.contract.content
    first.contract.enabled shouldEqual second.contract.enabled
  }
}