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
      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 99.vsys, 99.vsys)
      transferId <- sender.transfer(firstAddress, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1.vsys, 1.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 98.vsys, 98.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("Not able to create two aliases to same address") {
    val alias = "test_alias2"

    val f = for {
      _ <- assertBalances(firstAddress, 98.vsys, 98.vsys)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 97.vsys, 97.vsys)
      _ <- assertBadRequest(sender.createAlias(firstAddress, alias, 1.vsys))
    } yield succeed

    Await.result(f, 1.minute)
  }


  test("Not able to create two aliases to other addresses") {
    val alias = "test_alias3"

    val f = for {
      _ <- assertBalances(firstAddress, 97.vsys, 97.vsys)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 96.vsys, 96.vsys)
      _ <- assertBadRequest(sender.createAlias(secondAddress, alias, 1.vsys))
    } yield succeed

    Await.result(f, 1.minute)
  }
}
