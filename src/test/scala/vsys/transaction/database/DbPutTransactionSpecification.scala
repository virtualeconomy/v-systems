package vsys.blockchain.transaction.database

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction._

class DbPutTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("DbPutTransaction serialization roundtrip") {
    forAll(dbPutGen) { tx: DbPutTransaction =>
      require(tx.bytes.head == TransactionType.DbPutTransaction.id)
      val recovered = DbPutTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("DbPutTransaction serialization from TypedTransaction") {
    forAll(dbPutGen) { tx: DbPutTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[DbPutTransaction], tx)
    }
  }

  private def assertTxs(first: DbPutTransaction, second: DbPutTransaction): Unit = {
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.entry.data shouldEqual second.entry.data
    first.entry.dataType shouldEqual second.entry.dataType
    first.dbKey shouldEqual second.dbKey
    first.bytes shouldEqual second.bytes
  }
}
