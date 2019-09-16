package vsys.blockchain

import akka.actor.{ActorSystem, Props}
import vsys.settings.{BlockchainSettings, EventSettings, TestFunctionalitySettings, TestStateSettings}
import vsys.blockchain.state._
import vsys.account.PrivateKeyAccount
import vsys.blockchain.block.Block
import vsys.blockchain.block.SposConsensusBlockData
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{Transaction, TransactionParser}
import vsys.db.openDB
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus}
import vsys.events.{EventTriggers, EventWriterActor}

package object history {
  val MinInMemoryDiffSize = 5
  val DefaultBlockchainSettings = BlockchainSettings(
    addressSchemeCharacter = 'N',
    minimumInMemoryDiffSize = MinInMemoryDiffSize,
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = null,
    stateSettings = TestStateSettings.AllOn)

  val DefaultEventSetting = EventSettings(Seq.empty)
  val DefaultActorSys = ActorSystem().actorOf(Props[EventWriterActor])
  val triggerService = EventTriggers(DefaultActorSys, DefaultEventSetting)
  val db = openDB("./test/data", true)
  def domain(): Domain = {
    val (history, _, stateReader, blockchainUpdater) = StorageFactory(db, DefaultBlockchainSettings, triggerService, true)
    Domain(history, stateReader, blockchainUpdater)
  }

  private val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block = {
    Block.buildAndSign(
      version = 1: Byte,
      timestamp = 0L,
      reference = refTo,
      consensusData = SposConsensusBlockData(
        mintTime = 0L,
        mintBalance = 0L),
      transactionData = txs.map{tx: Transaction => ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)},
      signer = defaultSigner)
  }

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case (x :: xs) =>
        val block = buildBlockOfTxs(refTo, x)
        block +: chainBlocksR(block.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(randomSig, txs)
  }

  def malformSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))
}
