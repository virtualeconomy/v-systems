package vsys.blockchain.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.lease.LeaseCancelTransaction

import scala.util.Try

class LeaseCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {


  def parseBytes(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    require(bytes.head == TransactionType.LeaseCancelTransaction.id)
    LeaseCancelTransaction.parseTail(bytes.tail).get
  }

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[LeaseCancelTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseCancelTransaction, second: LeaseCancelTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.bytes shouldEqual second.bytes
  }

}
