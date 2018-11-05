package com.wavesplatform.history

import java.io.File
import java.util.concurrent.locks.{ReentrantReadWriteLock => RWL}

import com.wavesplatform.settings.VeeSettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{BlockChain, BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import org.iq80.leveldb.DB
import scorex.transaction.{BlockchainUpdater, History}
import vee.db.LevelDBWriter

import scala.util.{Success, Try}

object StorageFactory {

  private def createStateStorage(history: History, stateFile: Option[File]): Try[StateStorage] =
    StateStorage(stateFile, dropExisting = false).flatMap { ss =>
      if (ss.getHeight <= history.height()) Success(ss) else {
        ss.close()
        StateStorage(stateFile, dropExisting = true)
      }
    }

  def apply(settings: VeeSettings, db: DB): Try[(History with AutoCloseable, AutoCloseable, StateReader, BlockChain, BlockchainUpdater)] = {
    val lock = new RWL(true)
    val bcSetting = settings.blockchainSettings

    for {
      //TODO: old db (removed later)
      historyWriter <- HistoryWriterImpl(bcSetting.blockchainFile, lock)
      ss <- createStateStorage(historyWriter, bcSetting.stateFile)
      stateWriter = new StateWriterImpl(ss, lock)
      //TODO: new db
      chainState = new LevelDBWriter(db, bcSetting.functionalitySettings, settings.maxCacheSize, lock)
    } yield {
      val bcu = BlockchainUpdaterImpl(stateWriter, historyWriter, chainState, bcSetting.functionalitySettings, bcSetting.minimumInMemoryDiffSize, lock)
      (historyWriter, stateWriter, bcu.currentPersistedBlocksState, chainState, bcu)
    }
  }
}
