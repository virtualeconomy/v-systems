package vsys.api.http.spos

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import vsys.api.http._
import vsys.api.http.ApiMarshallers._
import vsys.blockchain.block.BlockGen
import vsys.blockchain.history.HistoryWriterImpl
import vsys.blockchain.state._
import vsys.blockchain.state.reader.StateReader
import vsys.db.openDB
import vsys.settings.FunctionalitySettings

class ConsensusRouteSpec extends RouteSpec("/consensus") with RestAPISettingsHelper with PropertyChecks with MockFactory with BlockGen with HistoryTest {
  private val state = mock[StateReader]

  private val db = openDB("./test/consensus/data", true)

  private val history = new HistoryWriterImpl(db, new ReentrantReadWriteLock(), true)
  appendGenesisBlock(history)
  for (i <- 1 to 10) appendTestBlock(history)

  private val route = SposConsensusApiRoute(restAPISettings, state, history, FunctionalitySettings.TESTNET).route

  routePath("/mintTime") - {
    "for existed block" in {
      val block = history.blockAt(3).get
      Get(routePath(s"/mintTime/${block.uniqueId.base58}")) ~> route ~> check {
        (responseAs[JsObject] \ "mintTime").as[Long] shouldEqual block.consensusData.mintTime
      }
    }

    "for not existed block" in {
      Get(routePath(s"/mintTime/brggwg4wg4g")) ~> route should produce(BlockNotExists)
    }
  }
}
