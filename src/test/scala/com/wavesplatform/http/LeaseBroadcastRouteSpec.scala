package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.{RequestGen, UtxPool}
import io.netty.channel.group.ChannelGroup
import org.scalacheck.Gen.posNum
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http._
import scorex.api.http.leasing.LeaseBroadcastApiRoute
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.GenericError


class LeaseBroadcastRouteSpec extends RouteSpec("/leasing/broadcast/") with RequestGen with PathMockFactory with PropertyChecks {
  private val settings = RestAPISettings.fromConfig(ConfigFactory.load())
  private val utx = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Left(TransactionValidationError(GenericError("foo"), t))).anyNumberOfTimes()

  "returns appropriate error code when validation fails for" - {
    val route = LeaseBroadcastApiRoute(settings, utx, allChannels).route

    "lease transaction" in forAll(leaseReq) { lease =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("lease"), v) ~> route

      forAll(nonPositiveLong) { q => posting(lease.copy(amount = q)) should produce(NegativeAmount) }
      forAll(invalidBase58) { pk => posting(lease.copy(senderPublicKey = pk)) should produce(InvalidAddress) }
      forAll(invalidBase58) { a => posting(lease.copy(recipient = a)) should produce(InvalidAddress) }
      forAll(nonPositiveLong) { fee => posting(lease.copy(fee = fee)) should produce(InsufficientFee) }
      forAll(posNum[Long]) { quantity => posting(lease.copy(amount = quantity, fee = Long.MaxValue)) should produce(OverflowError) }
    }

    "lease cancel transaction" in forAll(leaseCancelReq) { cancel =>
      def posting[A: Writes](v: A): RouteTestResult = Post(routePath("cancel"), v) ~> route

      forAll(invalidBase58) { pk => posting(cancel.copy(txId = pk)) should produce(CustomValidationError("invalid.leaseTx")) }
      forAll(invalidBase58) { pk => posting(cancel.copy(senderPublicKey = pk)) should produce(InvalidAddress) }
      forAll(nonPositiveLong) { fee => posting(cancel.copy(fee = fee)) should produce(InsufficientFee) }
    }
  }
}
