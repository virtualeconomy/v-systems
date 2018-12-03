package com.wavesplatform.it.transactions.debug

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class DebugPortfoliosSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  test("getting a balance considering pessimistic transactions from UTX pool - changed after UTX") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      portfolioBefore <- sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
      utxSizeBefore <- sender.utxSize

      _ <- sender.payment(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100)
      _ <- sender.waitForUtxIncreased(utxSizeBefore)

      portfolioAfter <- sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
    } yield {
      val expectedBalance = portfolioBefore.balance - 10.vsys // withdraw + fee
      assert(portfolioAfter.balance == expectedBalance)
    }

    Await.result(f, 1.minute)
  }

  test("getting a balance without pessimistic transactions from UTX pool - not changed after UTX") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      portfolioBefore <- sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
      utxSizeBefore <- sender.utxSize

      _ <- sender.payment(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100)
      _ <- sender.waitForUtxIncreased(utxSizeBefore)

      portfolioAfter <- sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    } yield {
      assert(portfolioAfter.balance == portfolioBefore.balance)
    }

    Await.result(f, 1.minute)
  }
}
