package vsys.blockchain

import vsys.blockchain.state.ByteStr
import vsys.blockchain.block.Block
import vsys.blockchain.transaction.{Transaction, ValidationError}
import vsys.utils.Synchronized

trait BlockchainUpdater extends Synchronized {
  def processBlock(block: Block): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, Seq[Transaction]]
}

