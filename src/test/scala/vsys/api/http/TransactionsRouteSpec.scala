package vsys.api.http

import akka.http.scaladsl.model.StatusCodes
import org.scalacheck.Gen._
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.crypto.encode.Base58
import vsys.account.Address
import vsys.api.http.ApiMarshallers._
import vsys.blockchain.UtxPool
import vsys.blockchain.block.BlockGen
import vsys.blockchain.history.History
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction._
import vsys.blockchain.transaction.lease._
import vsys.settings.TestStateSettings

class TransactionsRouteSpec extends RouteSpec("/transactions")
  with RestAPISettingsHelper with MockFactory with Matchers with TransactionGen with BlockGen with PropertyChecks {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  private val transactionsCount = 10

  private val history = mock[History]
  private val state = mock[StateReader]
  private val utx = mock[UtxPool]
  private val route = TransactionsApiRoute(restAPISettings, TestStateSettings.AllOn, state, history, utx).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  routePath("/address/{address}/limit/{limit}") - {
    "handles invalid address" in {
      forAll(bytes32gen, choose(1, MaxTransactionsPerRequest)) { case (bytes, limit) =>
        Get(routePath(s"/address/${Base58.encode(bytes)}/limit/$limit")) ~> route should produce(InvalidAddress)
      }
    }

    "handles invalid limit" in {
      forAll(accountGen, alphaStr.label("alphaNumericLimit")) { case (account, invalidLimit) =>
        Get(routePath(s"/address/${account.address}/limit/$invalidLimit")) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          (responseAs[JsObject] \ "message").as[String] shouldEqual "invalid.limit"
        }
      }

      forAll(accountGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) { case (account, limit) =>
        Get(routePath(s"/address/${account.address}/limit/$limit")) ~> route should produce(TooBigArrayAllocation)
      }
    }

    "working properly otherwise" ignore {
      val txs = for {
        count <- choose(1, transactionsCount)
        txs <- randomProvenTransactionsGen(count)
      } yield txs
      forAll(
        accountGen,
        choose(1, MaxTransactionsPerRequest),
        txs) { case (account, limit, txs) =>

        (state.accountTransactionIds _).expects(account: Address, limit, 0).returning((txs.size, txs.map(_.id))).once()
        txs.foreach { tx =>
          (state.transactionInfo _).expects(tx.id).returning(Some((1, ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)))).once()
          if (tx.isInstanceOf[LeaseCancelTransaction])
            (state.transactionInfo _).expects(tx.asInstanceOf[LeaseCancelTransaction].leaseId).returns(None).once()
        }
        Get(routePath(s"/address/${account.address}/limit/$limit")) ~> route ~> check {
          responseAs[Seq[Seq[JsValue]]].head.size shouldEqual txs.length.min(limit)
        }
      }
    }
  }

  routePath("/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx <- randomProvenTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) { case (tx, height) =>
        (state.transactionInfo _).expects(tx.id).returning(Some((height, ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)))).once()
        if (tx.isInstanceOf[LeaseCancelTransaction])
          (state.transactionInfo _).expects(tx.asInstanceOf[LeaseCancelTransaction].leaseId).returns(None).once()
        if (tx.isInstanceOf[LeaseTransaction])
          (state.isLeaseActive _).expects(tx.asInstanceOf[LeaseTransaction]).returns(true).once()
        Get(routePath(s"/info/${tx.id.base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsObject] - "lease" - "leaseStatus" shouldEqual ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx).json + ("height" -> JsNumber(height))
        }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomProvenTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.all _).expects().returns(txs).once()
        txs.foreach{ tx =>
          if (tx.isInstanceOf[LeaseCancelTransaction])
            (state.transactionInfo _).expects(tx.asInstanceOf[LeaseCancelTransaction].leaseId).returns(None).once()
        }
        Get(routePath("/unconfirmed")) ~> route ~> check {
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            (r \ "proofs").as[JsArray] shouldEqual JsArray(t.proofs.proofs.map(p => p.json))
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomProvenTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.size _).expects().returns(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomProvenTransactionGen) { tx =>
        (utx.transactionById _).expects(tx.id).returns(Some(tx)).once()
        if (tx.isInstanceOf[LeaseCancelTransaction])
          (state.transactionInfo _).expects(tx.asInstanceOf[LeaseCancelTransaction].leaseId).returns(None).once()
        Get(routePath(s"/unconfirmed/info/${tx.id.base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsObject] - "lease" shouldEqual tx.json
        }
      }
    }
  }

  routePath("/activeLeaseList/{address}") - {
    "handles invalid address in active lease check" in {
      forAll(bytes32gen) { case (bytes) =>
        Get(routePath(s"/activeLeaseList/${Base58.encode(bytes)}")) ~> route should produce(InvalidAddress)
      }
    }

  }

}
