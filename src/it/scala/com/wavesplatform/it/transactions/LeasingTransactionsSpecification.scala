package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class LeasingTransactionsSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("leasing vsys decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))

      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 10.vsys, fee = 10.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough vsys") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)

      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, 111.vsys, 10.vsys, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)
    } yield leaseFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough vsys for fee") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, 90.vsys, fee = 11.vsys, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }


  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    val f = for {
      _ <- assertBalances(firstAddress, 90.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70.vsys, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 85.vsys, 5.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 180.vsys)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBalances(firstAddress, 80.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("lease cancellation can be done only once") {
    val f = for {
      _ <- assertBalances(firstAddress, 80.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 75.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 115.vsys)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vsys, feeScale = 100).map(_.id))

      _ <- assertBalances(firstAddress, 70.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("only sender can cancel lease transaction") {
    val f = for {
      _ <- assertBalances(firstAddress, 70.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 110.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65.vsys, 50.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 115.vsys)

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = 1.vsys, feeScale = 100))
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough your vsys to self") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65.vsys, 50.vsys)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, 89.vsys, fee = 1.vsys, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65.vsys, 50.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }
}
