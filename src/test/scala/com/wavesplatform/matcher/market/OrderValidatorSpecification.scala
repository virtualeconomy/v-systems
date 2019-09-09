package com.wavesplatform.matcher.market

import com.wavesplatform.matcher.model._
import com.wavesplatform.matcher.{MatcherSettings, MatcherTestData}
import vsys.settings.{Constants, WalletSettings}
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import vsys.account.{PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{AssetInfo, ByteStr, LeaseInfo, Portfolio}
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.transaction.assets.IssueTransaction
import vsys.blockchain.transaction.assets.exchange.{AssetPair, Order}
import vsys.blockchain.UtxPool
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus, ValidationError}
import vsys.wallet.Wallet

class OrderValidatorSpecification extends WordSpec
  with PropertyChecks
  with Matchers
  with MatcherTestData
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with PathMockFactory {

  var storage = new OrderHistoryStorage(new MVStore.Builder().open())
  var oh = OrderHistoryImpl(storage)

  val utxPool: UtxPool = stub[UtxPool]

  val ss: StateReader = stub[StateReader]
  (ss.assetInfo _).when(*).returns(Some(AssetInfo(true, 10000000000L)))
  val i1: IssueTransaction = IssueTransaction.create(PrivateKeyAccount(Array.empty), "WBTC".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L).explicitGet()
  (ss.transactionInfo _).when(*).returns(Some((1, ProcessedTransaction(TransactionStatus.Success, i1.fee, i1))))

  val s: MatcherSettings = matcherSettings.copy(account = MatcherAccount.address)
  val w = Wallet(WalletSettings(None, "matcher", Some(WalletSeed)))
  val acc: Option[PrivateKeyAccount] = w.generateNewAccount()

  val matcherPubKey: PublicKeyAccount = w.findPrivateKey(s.account).explicitGet()

  private var ov = new OrderValidator {
    override val orderHistory: OrderHistory = oh
    override val utxPool: UtxPool = stub[UtxPool]
    override val settings: MatcherSettings = s
    override val wallet: Wallet = w
  }

  override protected def beforeEach(): Unit = {
    storage = new OrderHistoryStorage(new MVStore.Builder().open())
    ov = new OrderValidator {
      override val orderHistory: OrderHistory = oh
      override val utxPool: UtxPool = stub[UtxPool]
      override val settings: MatcherSettings = s
      override val wallet: Wallet = w
    }
  }

  val wbtc = ByteStr("WBTC".getBytes)
  val pairVsysBtc = AssetPair(None, Some(wbtc))

  "OrderValidator" should {
    "allows buy VSYS for BTC without balance for order fee" in {
      validateNewOrderTest(Portfolio(0, LeaseInfo.empty, Map(
        wbtc -> 10 * Constants.UnitsInVsys
      ))) shouldBe an[Right[_, _]]
    }

    "does not allow buy Vsys for BTC when assets number is negative" in {
      validateNewOrderTest(Portfolio(0, LeaseInfo.empty, Map(
        wbtc -> -10 * Constants.UnitsInVsys
      ))) shouldBe a[Left[_, _]]
    }
  }

  private def validateNewOrderTest(expectedPortfolio: Portfolio): Either[ValidationError.GenericError, Order] = {
    (ov.utxPool.portfolio _).when(*).returns(expectedPortfolio)
    val o = buy(
      pair = pairVsysBtc,
      price = 0.0022,
      amount = 100 * Constants.UnitsInVsys,
      matcherFee = Some((0.003 * Constants.UnitsInVsys).toLong)
    )
    ov.validateNewOrder(o)
  }
}
