package vsys.api.http.payment

import io.netty.channel.group.ChannelGroup
import io.netty.util.HashedWheelTimer
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import vsys.api.http.ApiMarshallers._
import vsys.api.http._
import vsys.account.PrivateKeyAccount
import vsys.blockchain.state._
import vsys.blockchain.UtxPool
import vsys.blockchain.transaction.{PaymentTransaction, Transaction, TransactionGen}
import vsys.utils.{Schedulers, Time}
import vsys.wallet.TestWallet

import scala.concurrent.duration._

class PaymentRouteSpec extends RouteSpec("/vsys/payment")
  with MockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with TransactionGen {

  private val utx = stub[UtxPool]
  (utx.putIfNew _).when(*).onCall((t: Transaction) => Right(true)).anyNumberOfTimes()
  private val allChannels = stub[ChannelGroup]
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "accepts payments" in {
    forAll(accountGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee"), feeScaleGen.label("feeScale"), attachmentGen.label("attachment")) {
      case (recipient: PrivateKeyAccount, amount: Long, fee: Long, feeScale, attachment: Array[Byte]) =>

        val timestamp = System.currentTimeMillis * 1000000L
        val time = mock[Time]
        (time.getTimestamp _).expects().returns(timestamp).anyNumberOfTimes()

        val sender = testWallet.privateKeyAccounts(0)
        val tx = PaymentTransaction.create(sender, recipient, amount, fee, feeScale, timestamp, attachment).explicitGet()

        val route = PaymentApiRoute(restAPISettings, testWallet, utx, allChannels, time,
          Schedulers.timeBoundedFixedPool(
            new HashedWheelTimer(),
            5.seconds,
            1,
            "rest-time-limited"
          )).route

        val req = Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee, "feeScale" -> feeScale, "attachment" -> Base58.encode(attachment))

        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "id").as[String] shouldEqual tx.id.toString
          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 2
          (resp \ "fee").as[Long] shouldEqual fee
          (resp \ "feeScale").as[Short] shouldEqual 100
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.timestamp
          (resp \ "attachment").as[String] shouldEqual Base58.encode(tx.attachment)
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
