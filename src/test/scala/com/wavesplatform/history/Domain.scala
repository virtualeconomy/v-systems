package vsys.blockchain.history

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.{BlockchainUpdater, History}

case class Domain(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater)
