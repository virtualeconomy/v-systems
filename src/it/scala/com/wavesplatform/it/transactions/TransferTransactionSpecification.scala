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

  test("asset transfer changes sender's and recipient's asset balance; issuer's.waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.vee, 100.vee)
      _ <- assertBalances(secondAddress, 100.vee, 100.vee)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, 2, reissuable = false, fee = 10.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90.vee, 90.vee)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      transferTransaction <- sender.transfer(firstAddress, secondAddress, defaultQuantity, fee = 10.vee, Some(issuedAssetId)).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferTransaction))

      _ <- assertBalances(firstAddress, 80.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 100.vee)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 0)
      _ <- assertAssetBalance(secondAddress, issuedAssetId, defaultQuantity)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("waves transfer changes waves balances and eff.b.") {
    val f = for {
      _ <- assertBalances(firstAddress, 80.vee, 80.vee)
      _ <- assertBalances(secondAddress, 100.vee, 100.vee)

      transferId <- sender.transfer(firstAddress, secondAddress, 5.vee, fee = 5.vee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
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
      1.vee,
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

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 104.vee, fee = 2.vee))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }


  test("can not make transfer without having enough of waves") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 106.vee, fee = 1.vee))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 70.vee, 70.vee)
      _ <- assertBalances(secondAddress, 105.vee, 105.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vee, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65.vee, 60.vee)
      _ <- assertBalances(secondAddress, 105.vee, 110.vee)

      transferFailureAssertion <- assertBadRequest(sender.transfer(firstAddress, secondAddress, 64.vee, fee = 1.vee))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65.vee, 60.vee)
      _ <- assertBalances(secondAddress, 105.vee, 110.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }

  test("can not make transfer without having enough of your own waves") {
    val f = for {
      fb <- traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65.vee, 60.vee)
      _ <- assertBalances(secondAddress, 105.vee, 110.vee)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.vee, fee = 5.vee, feeScale = 100).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 60.vee, 50.vee)
      _ <- assertBalances(secondAddress, 105.vee, 115.vee)

      transferFailureAssertion <- assertBadRequest(sender.transfer(secondAddress, firstAddress, 109.vee, fee = 1.vee))

      _ <- traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 60.vee, 50.vee)
      _ <- assertBalances(secondAddress, 105.vee, 115.vee)
    } yield transferFailureAssertion

    Await.result(f, 1.minute)
  }
}
