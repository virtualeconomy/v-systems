package scorex.lagonaki.mocks

import vsys.blockchain.state.ByteStr
import vsys.account.PrivateKeyAccount
import vsys.blockchain.block._
import vsys.blockchain.block.SposConsensusBlockData
import vsys.utils.crypto.EllipticCurveImpl
import vsys.blockchain.transaction.TransactionParser._
import vsys.blockchain.transaction.{Transaction, TransactionParser}
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus}

import scala.util.{Random, Try}

object TestBlock {

  private val defaultSigner = PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0))

  private val random: Random = new Random(10)

  def create(txs: Seq[Transaction]): Block = create(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs)

  def create(time: Long, txs: Seq[Transaction]): Block = sign(Block(
    timestamp = time,
    version = 1,
    reference = randomSignature(),
    signerData = SignerData(defaultSigner, ByteStr.empty),
    consensusData = SposConsensusBlockData(0L, 0L),
    transactionData = txs.map{tx: Transaction => ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)}))

  def createWithTxStatus(txs: Seq[Transaction], st: TransactionStatus.Value): Block = createWithTxStatus(time = Try(txs.map(_.timestamp).max).getOrElse(0), txs = txs, st)

  def createWithTxStatus(time: Long, txs: Seq[Transaction], st: TransactionStatus.Value): Block = sign(Block(
    timestamp = time,
    version = 1,
    reference = randomSignature(),
    signerData = SignerData(defaultSigner, ByteStr.empty),
    consensusData = SposConsensusBlockData(0L, 0L),
    transactionData = txs.map{tx: Transaction => ProcessedTransaction(st, tx.transactionFee, tx)}))

  def randomOfLength(length: Int): ByteStr = ByteStr(Array.fill(length)(random.nextInt().toByte))

  def randomSignature(): ByteStr = randomOfLength(SignatureLength)

  def withReference(ref: ByteStr): Block = sign(Block(0, 1, ref, SignerData(defaultSigner, ByteStr.empty),
    SposConsensusBlockData(0L, 0L), Seq.empty))

  private def sign(nonSignedBlock: Block): Block = {
    val toSign = nonSignedBlock.bytes
    val signature = EllipticCurveImpl.sign(defaultSigner, toSign)
    nonSignedBlock.copy(signerData = SignerData(defaultSigner, ByteStr(signature)))
  }
}
