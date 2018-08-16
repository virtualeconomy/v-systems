package vee.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._

class ChangeContractStatusTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ChangeContractStatusTransaction serialization roundtrip") {
    forAll(changeContractStatusGen) { tx: ChangeContractStatusTransaction =>
      require(tx.bytes.head == TransactionType.ChangeContractStatusTransaction.id)
      val recovered = ChangeContractStatusTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("ChangeContractStatusTransaction serialization from TypedTransaction") {
    forAll(changeContractStatusGen) { tx: ChangeContractStatusTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[ChangeContractStatusTransaction], tx)
    }
  }

  private def assertTxs(first: ChangeContractStatusTransaction, second: ChangeContractStatusTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.contractName shouldEqual second.contractName
    first.action shouldEqual second.action
  }
}