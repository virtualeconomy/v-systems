package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class AliasTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {
  test("Able to send money to an alias") {
    val alias = "test_alias"

    val f = for {
      _ <- assertBalances(firstAddress, 100.vee, 100.vee)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 99.vee, 99.vee)
      transferId <- sender.transfer(firstAddress, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1.vee, 1.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 98.vee, 98.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("Not able to create two aliases to same address") {
    val alias = "test_alias2"

    val f = for {
      _ <- assertBalances(firstAddress, 98.vee, 98.vee)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 97.vee, 97.vee)
      _ <- assertBadRequest(sender.createAlias(firstAddress, alias, 1.vee))
    } yield succeed

    Await.result(f, 1.minute)
  }


  test("Not able to create two aliases to other addresses") {
    val alias = "test_alias3"

    val f = for {
      _ <- assertBalances(firstAddress, 97.vee, 97.vee)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 96.vee, 96.vee)
      _ <- assertBadRequest(sender.createAlias(secondAddress, alias, 1.vee))
    } yield succeed

    Await.result(f, 1.minute)
  }
}
