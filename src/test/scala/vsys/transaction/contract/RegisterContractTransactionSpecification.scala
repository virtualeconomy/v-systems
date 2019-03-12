package vsys.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._

class RegisterContractTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("RegisterContractTransaction serialization roundtrip") {
    forAll(registerContractGen) { tx: RegisterContractTransaction =>
      require(tx.bytes.head == TransactionType.RegisterContractTransaction.id)
      val recovered = RegisterContractTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("RegisterContractTransaction serialization from TypedTransaction") {
    forAll(registerContractGen) { tx: RegisterContractTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[RegisterContractTransaction], tx)
    }
  }

  private def assertTxs(first: RegisterContractTransaction, second: RegisterContractTransaction): Unit = {
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.contractId.bytes.arr shouldEqual second.contractId.bytes.arr
    first.contract.descriptor.toArray shouldEqual second.contract.descriptor.toArray
    first.contract.initializer shouldEqual second.contract.initializer
    first.contract.languageCode shouldEqual second.contract.languageCode
    first.contract.languageVersion shouldEqual second.contract.languageVersion
    first.data.flatMap(_.bytes).toArray shouldEqual second.data.flatMap(_.bytes).toArray
    first.description shouldEqual second.description
  }
}