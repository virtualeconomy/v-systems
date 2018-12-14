package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction.{BlockchainUpdater, History}

import scala.util.{Success, Try}
import org.iq80.leveldb.DB

object StorageFactory {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(db: DB, settings: BlockchainSettings): Try[(History, AutoCloseable, StateReader, BlockchainUpdater)] = {
    val lock = new RWL(true)
    val historyWriter = new HistoryWriterImpl(db, lock)
    for {
      ss <- createStateStorage(historyWriter, settings.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
      (historyWriter, stateWriter, bcu.currentPersistedBlocksState, bcu)
    }
  }
}
