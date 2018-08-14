package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class PaymentTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("waves payment changes waves balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vee, 100.vee)
      _ <- assertBalances(secondAddress, 100.vee, 100.vee)

      transferId <- sender.payment(firstAddress, secondAddress, 5.vee, fee = 5.vee, feeScale = 100)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 90.vee, 90.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }
}
