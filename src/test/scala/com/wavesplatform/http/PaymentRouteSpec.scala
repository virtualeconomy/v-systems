package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.{TestWallet, TransactionGen, UtxPool}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}
import scorex.api.http.ApiKeyNotValid
import vee.api.http.vee.PaymentApiRoute
import scorex.transaction.Transaction
import scorex.transaction.PaymentTransaction
import scorex.utils.Time

class PaymentRouteSpec extends RouteSpec("/vee/payment")
  with MockFactory with PropertyChecks with RestAPISettingsHelper with TestWallet with TransactionGen {

  private val utx = stub[UtxPool]
  (utx.putIfNew _).when(*).onCall((t: Transaction) => Right(true)).anyNumberOfTimes()
  private val allChannels = stub[ChannelGroup]
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  "accepts payments" in {
    forAll(accountGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee"), feeScaleGen.label("feeScale")) {
      case (recipient, amount, fee, feeScale) =>

        val timestamp = System.currentTimeMillis * 1000000L
        val time = mock[Time]
        (time.getTimestamp _).expects().returns(timestamp).anyNumberOfTimes()

        val sender = testWallet.privateKeyAccounts.head
        val tx = PaymentTransaction.create(sender, recipient, amount, fee, feeScale, timestamp)

        val route = PaymentApiRoute(restAPISettings, testWallet, utx, allChannels, time).route

        val req = Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee, "feeScale" -> feeScale)

        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "id").as[String] shouldEqual tx.right.get.id.toString
          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 2
          (resp \ "fee").as[Long] shouldEqual fee
          (resp \ "feeScale").as[Short] shouldEqual 100
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.right.get.timestamp
          (resp \ "sender").as[String] shouldEqual sender.address
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
