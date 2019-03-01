package vsys.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.TransactionParser
import scorex.transaction.TransactionParser.TransactionType

import scala.util.Try

class ExecuteContractTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def parseBytes(bytes: Array[Byte]): Try[ExecuteContractTransaction] = Try {
    require(bytes.head == TransactionType.ExecuteContractTransaction.id)
    ExecuteContractTransaction.parseTail(bytes.tail).get
  }

  property("ExecuteContractTransaction serialization roundtrip") {
    forAll(executeContractGen) { tx: ExecuteContractTransaction =>
      val recovered = parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("ExecuteContractTransaction serialization from TypedTransaction") {
    forAll(executeContractGen) { tx: ExecuteContractTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[ExecuteContractTransaction], tx)
    }
  }

  private def assertTxs(first: ExecuteContractTransaction, second: ExecuteContractTransaction): Unit = {
    first.contractId.bytes.arr shouldEqual second.contractId.bytes.arr
    first.entryPoints shouldEqual second.entryPoints
    first.dataStack.flatMap(_.bytes).toArray shouldEqual second.dataStack.flatMap(_.bytes).toArray
    first.description shouldEqual second.description
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.bytes shouldEqual second.bytes
  }
}
