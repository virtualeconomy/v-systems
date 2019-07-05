package vsys.blockchain.state

import vsys.blockchain.history.HistoryWriterImpl
import scorex.lagonaki.mocks.TestBlock
import vsys.blockchain.transaction.TransactionParser.SignatureLength

trait HistoryTest {
  def appendGenesisBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte))))(Right(BlockDiff.empty)).explicitGet()
  def appendTestBlock(history: HistoryWriterImpl): Unit =
    history.appendBlock(TestBlock.withReference(history.lastBlock.get.uniqueId))(Right(BlockDiff.empty)).explicitGet()
}
