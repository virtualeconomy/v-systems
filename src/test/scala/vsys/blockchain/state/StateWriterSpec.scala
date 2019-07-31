package vsys.blockchain.state

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import vsys.db.openDB
import vsys.settings.TestStateSettings

class StateWriterSpec extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  test("increase height when applying block diff") {

    val db = openDB("./test/statewriter/data", true)
    val storage = StateStorage(db, dropExisting = false)
    val writer = new StateWriterImpl(storage, new ReentrantReadWriteLock(), TestStateSettings.AllOn)
    forAll(Gen.choose(0, Int.MaxValue)) { heightDiff =>
      val h = writer.height
      writer.applyBlockDiff(BlockDiff(Diff.empty, heightDiff, Map.empty))
      writer.height shouldBe h + heightDiff
      storage.getHeight shouldBe h + heightDiff
    }
  }
}
