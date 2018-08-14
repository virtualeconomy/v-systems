package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class ReissueTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  private val defaultQuantity = 100000

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vee, 100.vee)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, 2, reissuable = true, fee = 10.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90.vee, 90.vee)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      reissuedAssetId <- sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = 10.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(reissuedAssetId))

      _ <- assertBalances(firstAddress, 80.vee, 80.vee)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 2 * defaultQuantity)
    } yield succeed

    Await.result(f, 1.minute)
  }

  // todo can't reissue not reissuable asset
}
