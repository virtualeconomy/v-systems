package com.wavesplatform.history

import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import scorex.transaction.{BlockchainUpdater, History}

import org.iq80.leveldb.DB

object StorageFactory {

  private def createStateStorage(history: History, db:DB): StateStorage = {
    val ss = StateStorage(db, dropExisting = false)
    if (ss.getHeight <= history.height()) ss
    else StateStorage(db, dropExisting = true)
  }

  def apply(db: DB, settings: BlockchainSettings): (History, AutoCloseable, StateReader, BlockchainUpdater) = {
    val lock = new RWL(true)
    val historyWriter = new HistoryWriterImpl(db, lock)
    val ss = createStateStorage(historyWriter, db)
    val stateWriter = new StateWriterImpl(ss, lock)
    val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
    (historyWriter, stateWriter, bcu.currentPersistedBlocksState, bcu)
  }
}
