package com.wavesplatform.state2

import com.wavesplatform.state2.reader.{LeaseDetails, StateReader}
import scorex.account.Address
import scorex.block.Block
import scorex.transaction.HistoryWriter

trait BlockChain extends HistoryWriter with StateReader {
  def score: BigInt
  def lastBlock: Option[Block]
  def transactionHeight(id: ByteStr): Option[Int]
  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]
  def blockBytes(blockId: ByteStr): Option[Array[Byte]]
  def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]]
  def parent(block: Block, back: Int): Option[Block]
  def collectSposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A]
  def veeDistribution(height: Int): Map[Address, Long]
  def rollbackTo(targetBlockId: ByteStr): Seq[Block]
}
