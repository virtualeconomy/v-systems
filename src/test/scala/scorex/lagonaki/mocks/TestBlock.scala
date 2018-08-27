package scorex.lagonaki.mocks

import com.wavesplatform.state2.ByteStr
import scorex.account.PrivateKeyAccount
import scorex.block._
import vee.consensus.spos.SposConsensusBlockData
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction.{ProcessedTransaction, Transaction, TransactionParser, TransactionStatus}

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
