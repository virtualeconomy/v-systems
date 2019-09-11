package vsys.utils

import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.database._
import vsys.blockchain.transaction.spos._
import vsys.blockchain.transaction.lease._
import vsys.blockchain.transaction._

class TestTransactionHelper extends FlatSpec with Matchers with MockitoSugar {
  "TransactionHelper" should "return expected amount and fee" in {
    val p = mock[PaymentTransaction]
    val dbTx = mock[DbPutTransaction]
    val lct = mock[LeaseCancelTransaction]
    val cst = mock[ContendSlotsTransaction]
    val rst = mock[ReleaseSlotsTransaction]
    val lt = mock[LeaseTransaction]
    val gt = mock[GenesisTransaction]
    val mt = mock[MintingTransaction]
    val ecft = mock[ExecuteContractFunctionTransaction]
    val rct = mock[RegisterContractTransaction]
    val tx = mock[ProcessedTransaction]


    when(p.amount).thenReturn(1)
    when(p.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(p)
    TransactionHelper.extractAmtFee(tx) shouldBe((1, 2))

    when(dbTx.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(dbTx)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))

    when(lct.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(lct)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))

    when(cst.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(cst)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))

    when(rst.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(rst)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))

    when(lt.transactionFee).thenReturn(2)
    when(lt.amount).thenReturn(1)
    when(tx.transaction).thenReturn(lt)
    TransactionHelper.extractAmtFee(tx) shouldBe((1, 2))

    when(gt.amount).thenReturn(1)
    when(tx.transaction).thenReturn(gt)
    TransactionHelper.extractAmtFee(tx) shouldBe((1, 0))

    when(mt.amount).thenReturn(1)
    when(tx.transaction).thenReturn(mt)
    TransactionHelper.extractAmtFee(tx) shouldBe((1, 0))

    when(ecft.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(ecft)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))

    when(rct.transactionFee).thenReturn(2)
    when(tx.transaction).thenReturn(rct)
    TransactionHelper.extractAmtFee(tx) shouldBe((0, 2))
  }
}