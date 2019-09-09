package vsys.blockchain.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.spos.ContendSlotsTransaction

class ContendSlotsTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("ContendSlotstransaction serialization roundtrip") {
    forAll(contendSlotsGen) { tx: ContendSlotsTransaction =>
      require(tx.bytes.head == TransactionType.ContendSlotsTransaction.id)
      val recovered = ContendSlotsTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("ContendSlotstransaction serialization from TypedTransaction") {
    forAll(contendSlotsGen) { tx: ContendSlotsTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[ContendSlotsTransaction], tx)
    }
  }

  private def assertTxs(first: ContendSlotsTransaction, second: ContendSlotsTransaction): Unit = {
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.timestamp shouldEqual second.timestamp
    first.transactionFee shouldEqual second.transactionFee
    first.feeScale shouldEqual second.feeScale
    first.slotId shouldEqual second.slotId
    first.bytes shouldEqual second.bytes
  }
}
