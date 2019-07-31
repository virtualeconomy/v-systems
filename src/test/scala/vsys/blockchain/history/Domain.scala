package vsys.blockchain.history

import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.BlockchainUpdater

case class Domain(history: History, stateReader: StateReader, blockchainUpdater: BlockchainUpdater)
