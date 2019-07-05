package vsys.blockchain.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.TransactionParser.TransactionType

import scala.util.Try

class ExecuteContractFunctionTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def parseBytes(bytes: Array[Byte]): Try[ExecuteContractFunctionTransaction] = Try {
    require(bytes.head == TransactionType.ExecuteContractFunctionTransaction.id)
    ExecuteContractFunctionTransaction.parseTail(bytes.tail).get
  }

  property("ExecuteContractTransaction serialization roundtrip") {
    forAll(executeContractGen) { tx: ExecuteContractFunctionTransaction =>
      val recovered = parseBytes(tx.bytes).get

      assertTxs(recovered, tx)
    }
  }

  property("ExecuteContractTransaction serialization from TypedTransaction") {
    forAll(executeContractGen) { tx: ExecuteContractFunctionTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get

      assertTxs(recovered.asInstanceOf[ExecuteContractFunctionTransaction], tx)
    }
  }

  private def assertTxs(first: ExecuteContractFunctionTransaction, second: ExecuteContractFunctionTransaction): Unit = {
    first.contractId.bytes.arr shouldEqual second.contractId.bytes.arr
    first.funcIdx shouldEqual second.funcIdx
    first.data.flatMap(_.bytes).toArray shouldEqual second.data.flatMap(_.bytes).toArray
    first.attachment shouldEqual second.attachment
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.bytes shouldEqual second.bytes
  }
}
