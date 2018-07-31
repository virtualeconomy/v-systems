package vee.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._
import vee.transaction.spos.ContendSlotsTransaction

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
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.slotid shouldEqual second.slotid
    first.bytes shouldEqual second.bytes
  }
}
