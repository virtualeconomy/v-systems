package vsys.utils

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.blockchain.transaction._

class TestTransactionHelper extends PropSpec with Matchers with GeneratorDrivenPropertyChecks with PropertyChecks with TransactionGen {
  property("TransactionHelper should return expected amount and fee") {
    forAll(randomProcessedTransactionGen) {
      case (pTx: ProcessedTransaction ) => pTx.transaction match {
        case tx1: NonFeeTransaction with AmountInvolved => TransactionHelper.extractAmtFee(pTx) shouldBe((tx1.amount, 0))
        case tx2: AmountInvolved => TransactionHelper.extractAmtFee(pTx) shouldBe((tx2.amount, tx2.transactionFee))
        case tx3: ProvenTransaction => TransactionHelper.extractAmtFee(pTx) shouldBe((0, tx3.transactionFee))
      }
    }
  }
}