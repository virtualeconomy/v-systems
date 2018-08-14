package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class LeasingTransactionsSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("leasing vee decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))

      _ <- assertBalances(firstAddress, 100.vee, 100.vee)
      _ <- assertBalances(secondAddress, 100.vee, 100.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 10.vee, fee = 10.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough vee") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)

      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, 111.vee, 10.vee, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)
    } yield leaseFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough vee for fee") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, 90.vee, fee = 11.vee, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }


  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    val f = for {
      _ <- assertBalances(firstAddress, 90.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70.vee, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 85.vee, 5.vee)
      _ <- assertBalances(secondAddress, 100.vee, 180.vee)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBalances(firstAddress, 80.vee, 70.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("lease cancellation can be done only once") {
    val f = for {
      _ <- assertBalances(firstAddress, 80.vee, 70.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vee, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 75.vee, 60.vee)
      _ <- assertBalances(secondAddress, 100.vee, 115.vee)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.vee, feeScale = 100).map(_.id))

      _ <- assertBalances(firstAddress, 70.vee, 60.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("only sender can cancel lease transaction") {
    val f = for {
      _ <- assertBalances(firstAddress, 70.vee, 60.vee)
      _ <- assertBalances(secondAddress, 100.vee, 110.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vee, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65.vee, 50.vee)
      _ <- assertBalances(secondAddress, 100.vee, 115.vee)

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = 1.vee, feeScale = 100))
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("can not make leasing without having enough your vee to self") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65.vee, 50.vee)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, 89.vee, fee = 1.vee, feeScale = 100))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65.vee, 50.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }
}
