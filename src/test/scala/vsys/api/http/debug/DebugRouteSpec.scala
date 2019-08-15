package vsys.api.http.debug

import java.util.concurrent.ConcurrentMap

import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import vsys.api.http.ApiMarshallers._
import vsys.api.http._
import vsys.blockchain.{BlockchainUpdater, UtxPool}
import vsys.blockchain.block.BlockGen
import vsys.blockchain.history.History
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{LeaseInfo, Portfolio}
import vsys.blockchain.transaction.TransactionGen
import vsys.network.{PeerDatabase, PeerInfo}
import vsys.wallet.TestWallet

class DebugRouteSpec
  extends RouteSpec("/debug")
    with RestAPISettingsHelper with TestWallet with MockFactory with PropertyChecks with TransactionGen with BlockGen {

  private val state = mock[StateReader]
  private val history = mock[History]
  private val peerDatabase = mock[PeerDatabase]
  private val channelGroup = mock[ChannelGroup]
  private val utxPool = mock[UtxPool]
  private val establishedConnections = mock[ConcurrentMap[Channel, PeerInfo]]
  private val route = DebugApiRoute(restAPISettings, testWallet, state, history, peerDatabase, establishedConnections, mock[BlockchainUpdater], channelGroup, utxPool).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  routePath("/state") in {
    val portfolioGen = for {
      a <- accountGen
      b <- Gen.posNum[Long]
    } yield a.toAddress -> Portfolio(b, LeaseInfo.empty, Map.empty)

    forAll(Gen.chooseNum(0, 20).flatMap(n => Gen.listOfN(n, portfolioGen))) { portfolios =>
      val portfolioMap = portfolios.toMap
      (state.accountPortfolios _).expects().returning(portfolioMap).once()

      Get(routePath("/state")) ~> api_key(apiKey) ~> route ~> check {
        responseAs[JsObject] shouldEqual JsObject(portfolios.map {
          case (account, p) => account.address -> JsNumber(p.balance)
        })
      }
    }
  }

  routePath("/info") in {
    forAll(Gen.posNum[Int]) { height =>
      (state.height _).expects().returning(height).once()
      (state.accountPortfolios _).expects().returning(Map.empty).once()
      Get(routePath("/info")) ~> api_key(apiKey) ~> route ~> check {
        responseAs[JsObject] should have(
          "stateHeight" -> JsNumber(height),
          "stateHash".ofType[JsNumber]
        )
      }
    }
  }
}
