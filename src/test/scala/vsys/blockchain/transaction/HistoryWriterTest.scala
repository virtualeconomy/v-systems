package vsys.blockchain.transaction

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.scalatest.{FunSuite, Matchers}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.history.HistoryWriterImpl
import vsys.blockchain.state._
import vsys.db.openDB

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.DynamicVariable

class HistoryWriterTest extends FunSuite with Matchers with HistoryTest {

  test("concurrent access to lastBlock doesn't throw any exception") {
    val db = openDB("./test/historywriter/data", true)
    val history = new HistoryWriterImpl(db, new ReentrantReadWriteLock(), true)
    appendGenesisBlock(history)

    (1 to 1000).foreach { _ =>
      appendTestBlock(history)
    }

    val failed = new DynamicVariable(false)

    def tryAppendTestBlock(history: HistoryWriterImpl): Either[ValidationError, BlockDiff] =
      history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId))(Right(BlockDiff.empty))

    (1 to 1000).foreach { _ =>
      Future(tryAppendTestBlock(history)).recover[Any] { case e => e.printStackTrace(); failed value_= true }
      Future(history.discardBlock()).recover[Any] { case e => e.printStackTrace(); failed value_= true }
    }
    Thread.sleep(1000)

    failed.value shouldBe false
  }
}
