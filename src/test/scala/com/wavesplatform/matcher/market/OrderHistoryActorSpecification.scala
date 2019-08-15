package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import vsys.blockchain.UtxPool
import com.wavesplatform.matcher.market.OrderHistoryActor.GetOrderHistory
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import vsys.settings.WalletSettings
import vsys.blockchain.state.ByteStr
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
import vsys.blockchain.transaction.assets.exchange.AssetPair
import vsys.utils.{NTP, ScorexLogging}
import vsys.wallet.Wallet

class OrderHistoryActorSpecification extends TestKit(ActorSystem("MatcherTest"))
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender
  with MatcherTestData
  with BeforeAndAfterEach
  with ScorexLogging
  with PathMockFactory {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val settings: MatcherSettings = matcherSettings.copy(account = MatcherAccount.address)
  val pair = AssetPair(Some(ByteStr("BTC".getBytes)), Some(ByteStr("VSYS".getBytes)))
  val utxPool: UtxPool = stub[UtxPool]
  val wallet = Wallet(WalletSettings(None, "matcher", Some(WalletSeed)))
  wallet.generateNewAccount()

  var actor: ActorRef = system.actorOf(Props(new OrderHistoryActor(settings, utxPool, wallet)))

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    actor = system.actorOf(Props(new OrderHistoryActor(settings, utxPool, wallet)))
  }
  "OrderHistoryActor" should {

    "not process expirable messages" in {
      val r = GetOrderHistory(pair, "address", NTP.correctedTime() - OrderHistoryActor.RequestTTL - 1)
      actor ! r
      expectNoMessage()
    }
  }
}
