package vee.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._

class MintingTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Mintingtransaction serialization roundtrip") {
    forAll(MintingGen) { tx: MintingTransaction =>
      require(tx.bytes.head == TransactionType.MintingTransaction.id)
      val recovered = MintingTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("Mintingtransaction serialization from TypedTransaction") {
    forAll(MintingGen) { tx: MintingTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[MintingTransaction], tx)
    }
  }

  private def assertTxs(first: MintingTransaction, second: MintingTransaction): Unit = {
    first.recipient.address shouldEqual second.recipient.address
    first.timestamp shouldEqual second.timestamp
    first.amount shouldEqual second.amount
    first.currentBlockHeight shouldEqual second.currentBlockHeight
    first.bytes shouldEqual second.bytes
  }
}
