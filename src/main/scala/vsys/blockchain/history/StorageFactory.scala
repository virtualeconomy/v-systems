package vsys.blockchain.history

import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import org.iq80.leveldb.DB
import vsys.blockchain.BlockchainUpdater
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl, StateWriter}
import vsys.settings.BlockchainSettings

object StorageFactory {

  private def createStateStorage(history: History, db: DB): StateStorage = {
    val ss = StateStorage(db, dropExisting = false)
    if (ss.getHeight <= history.height()) ss
    else StateStorage(db, dropExisting = true)
  }

  def apply(db: DB, settings: BlockchainSettings, renew: Boolean = false): (History, StateWriter, StateReader, BlockchainUpdater) = {
    val lock = new RWL(true)
    val historyWriter = new HistoryWriterImpl(db, lock, renew)
    val ss = createStateStorage(historyWriter, db)
    val stateWriter = new StateWriterImpl(ss, lock, settings.stateSettings)
    val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, settings.functionalitySettings, settings.minimumInMemoryDiffSize, lock)
    (historyWriter, stateWriter, bcu.currentPersistedBlocksState, bcu)
  }
}
