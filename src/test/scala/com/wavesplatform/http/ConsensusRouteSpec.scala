package com.wavesplatform.http

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.BlockGen
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import scorex.api.http.BlockNotExists
import vsys.consensus.spos.api.http.SposConsensusApiRoute
import vsys.db.openDB

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
