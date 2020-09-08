package vsys.blockchain.transaction

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.lease.LeaseTransaction

import scala.util.Try

class LeaseTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def parseBytes(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    require(bytes.head == TransactionType.LeaseTransaction.id)
    LeaseTransaction.parseTail(bytes.tail).get
  }


  property("Lease transaction serialization roundtrip") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("Lease transaction from TransactionParser") {
    forAll(leaseGen) { tx: LeaseTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[LeaseTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseTransaction, second: LeaseTransaction): Unit = {
    first.recipient.stringRepr shouldEqual second.recipient.stringRepr
    first.amount shouldEqual second.amount
    first.transactionFee shouldEqual second.transactionFee
    first.feeScale shouldEqual second.feeScale
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.bytes shouldEqual second.bytes
  }
}
