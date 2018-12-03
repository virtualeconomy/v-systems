package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import scorex.account.{AddressOrAlias, PrivateKeyAccount}
import scorex.api.http.Mistiming
import scorex.api.http.assets.SignedTransferRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.{sequence, traverse}
import scala.concurrent.duration._

class TransferTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  private val defaultQuantity = 100000

  test("asset transfer changes sender's and recipient's asset balance; issuer's.vsys balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vsys, 100.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, 2, reissuable = false, fee = 10.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90.vsys, 90.vsys)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      transferTransaction <- sender.transfer(firstAddress, secondAddress, defaultQuantity, fee = 10.vsys, Some(issuedAssetId)).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferTransaction))

      _ <- assertBalances(firstAddress, 80.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 0)
      _ <- assertAssetBalance(secondAddress, issuedAssetId, defaultQuantity)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("vsys transfer changes vsys balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 80.vsys, 80.vsys)
      _ <- assertBalances(secondAddress, 100.vsys, 100.vsys)

      transferId <- sender.transfer(firstAddress, secondAddress, 5.vsys, fee = 5.vsys).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("invalid signed vsys transfer should not be in UTX or blockchain") {
    def createSignedTransferRequest(tx: TransferTransaction): SignedTransferRequest = {
      import tx._
      SignedTransferRequest(
        Base58.encode(tx.sender.publicKey),
        assetId.map(_.base58),
        recipient.stringRepr,
        amount,
        fee,
        feeAssetId.map(_.base58),
        timestamp,
        attachment.headOption.map(_ => Base58.encode(attachment)),
        signature.base58
      )
    }

    val invalidByTsTx = TransferTransaction.create(None,
      PrivateKeyAccount(Base58.decode(sender.accountSeed).get),
      AddressOrAlias.fromString(sender.address).right.get,
      1,
      System.currentTimeMillis() + (1.day).toMillis,
      None,
      1.vsys,
      Array.emptyByteArray
    ).right.get

    val invalidTxId = invalidByTsTx.id

    val invalidByTsSignedRequest = createSignedTransferRequest(invalidByTsTx)

    val f = for {
      _ <- expectErrorResponse(sender.signedTransfer(invalidByTsSignedRequest)) { x =>
        x.error == Mistiming.Id
      }
      _ <- sequence(allNodes.map(_.ensureTxDoesntExist(invalidTxId.base58)))
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("can not make transfer without having enough of fee") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 104.vsys, fee = 2.vsys))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }


  test("can not make transfer without having enough of vsys") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 106.vsys, fee = 1.vsys))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70.vsys, 70.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 105.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 110.vsys)

      transferFailureAssertion <- assertBadRequest(sender.transfer(firstAddress, secondAddress, 64.vsys, fee = 1.vsys))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 110.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make transfer without having enough of your own vsys") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65.vsys, 60.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 110.vsys)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vsys, fee = 5.vsys, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 60.vsys, 50.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 115.vsys)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 109.vsys, fee = 1.vsys))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 60.vsys, 50.vsys)
      _ <- assertBalances(secondAddress, 105.vsys, 115.vsys)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }
}
