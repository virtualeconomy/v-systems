package vsys.it.transactions

import vsys.it.util._
import vsys.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class PaymentTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("vsys payment changes vsys balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      transfer <- sender.payment(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transfer.id))

      _ <- assertBalances(firstAddress, 90.vsys, 90.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }
}
