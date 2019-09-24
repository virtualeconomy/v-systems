package vsys.blockchain.consensus.nxt

import org.scalatest.{Assertions, Matchers, PropSpec}
import vsys.account.{Address, PrivateKeyAccount}
import vsys.blockchain.consensus.TransactionsOrdering
import vsys.blockchain.state._
import vsys.blockchain.transaction.PaymentTransaction
import vsys.utils.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val txsDifferentById = (0 to 3).map(i =>
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 5, 100, 125L, Array(i.toByte)).explicitGet).sortBy(t => t.id.base58)

    val correctSeq = txsDifferentById ++ Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 2, 100, 124L, Array.empty).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 125L, Array.empty).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 124L, Array.empty).explicitGet)

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val txsDifferentById = (0 to 3).map(i =>
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 5, 100, 125L, Array(i.toByte)).explicitGet).sortBy(t => t.id.base58)

    val correctSeq = txsDifferentById ++ Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 2, 100, 123L, Array.empty).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 123L, Array.empty).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 124L, Array.empty).explicitGet)

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 124L, Array[Byte](2, 3, 5)).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 123L, Array[Byte](2, 3, 5)).explicitGet)

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 123L, Array[Byte](2, 3, 5)).explicitGet,
      PaymentTransaction.create(PrivateKeyAccount(Array.fill(32)(0)), Address.fromString("ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt").explicitGet, 100000, 1, 100, 124L, Array[Byte](2, 3, 5)).explicitGet)
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
