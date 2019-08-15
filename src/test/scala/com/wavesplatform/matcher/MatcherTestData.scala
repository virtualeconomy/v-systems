package com.wavesplatform.matcher

import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.model.{BuyLimitOrder, SellLimitOrder}
import vsys.settings.loadConfig
import vsys.blockchain.state.ByteStr
import org.scalacheck.{Arbitrary, Gen}
import vsys.account.PrivateKeyAccount
import vsys.utils.crypto.hash.SecureCryptographicHash
import vsys.blockchain.transaction.assets.exchange.{AssetPair, Order, OrderType}
import vsys.utils.NTP

trait MatcherTestData {
  private val signatureSize = 32

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(signatureSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  val WalletSeed = ByteStr("Matcher".getBytes()).toString
  val MatcherSeed = SecureCryptographicHash((0.toString + WalletSeed).getBytes("UTF-8"))
  val MatcherAccount = PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long] = Gen.choose(1, Long.MaxValue)

  val vsysAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)
  val assetIdGen: Gen[Option[Array[Byte]]] = Gen.frequency((1, vsysAssetGen), (10, bytes32gen.map(Some(_))))

  val assetPairGen = Gen.zip(assetIdGen, assetIdGen).
    suchThat(p => p._1 != p._2).
    map(p => AssetPair(p._1.map(vsys.blockchain.state.ByteStr(_)), p._2.map(vsys.blockchain.state.ByteStr(_))))

  val maxTimeGen: Gen[Long] = Gen.choose(10000L, Order.MaxLiveTime).map(_ + NTP.correctedTime())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(NTP.correctedTime() - _)

  val config = loadConfig(ConfigFactory.parseString(
    """vsys {
      |  directory: "/tmp/vsys-test"
      |  matcher {
      |    enable: yes
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 9925
      |    order-history-file: null
      |    min-order-fee: 100000
      |    order-match-tx-fee: 100000
      |    snapshots-interval: 1d
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2"]
      |    predefined-pairs: [{amountAsset = "BASE2", priceAsset = "BASE1"}]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[F,f]orbidden"]
      |  }
      |}""".stripMargin))

  val matcherSettings = MatcherSettings.fromConfig(config)

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxVsysAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair, price: Long, amount: Long, sender: Option[PrivateKeyAccount] = None,
                   matcherFee: Option[Long] = None): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxVsysAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def sellGenerator(pair: AssetPair, price: Long, amount: Long, sender: Option[PrivateKeyAccount] = None,
                    matcherFee: Option[Long] = None): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxVsysAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def buy(pair: AssetPair, price: BigDecimal, amount: Long, sender: Option[PrivateKeyAccount] = None,
          matcherFee: Option[Long] = None): Order =
    valueFromGen(buyGenerator(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee))._1

  def sell(pair: AssetPair, price: BigDecimal, amount: Long,sender: Option[PrivateKeyAccount] = None,
           matcherFee: Option[Long] = None): Order =
    valueFromGen(sellGenerator(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    orderType <- orderTypeGenerator
    price: Long <- maxVsysAmountGen
    amount: Long <- maxVsysAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxVsysAmountGen
  } yield (Order(sender, MatcherAccount, pair, orderType, price, amount, timestamp, expiration, matcherFee), sender)

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxVsysAmountGen
    amount: Long <- maxVsysAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxVsysAmountGen
  } yield BuyLimitOrder(price, amount, Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair <- assetPairGen
    price: Long <- maxVsysAmountGen
    amount: Long <- maxVsysAmountGen
    timestamp: Long <- createdTimeGen
    expiration: Long <- maxTimeGen
    matcherFee: Long <- maxVsysAmountGen
  } yield SellLimitOrder(price, amount, Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

}
