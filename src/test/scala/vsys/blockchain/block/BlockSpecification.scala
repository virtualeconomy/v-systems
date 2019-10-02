package vsys.blockchain.block

import java.security.SecureRandom

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction._

class BlockSpecification extends FunSuite with Matchers with MockFactory {

  test(" block with txs bytes/parse roundtrip") {

    val reference = Array.fill(Block.BlockIdLength)(new SecureRandom().nextInt(100).toByte)
    val gen = PrivateKeyAccount(reference)

    val mt = System.currentTimeMillis() / 10000 * 10000000000000L
    val mb = 100000000000L


    val ts = System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L - 5000000000L
    val sender = PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction.create(sender, gen, 5, 1000, 100, ts, Array()).right.get

    val tbd = Seq(tx)
    val cbd = SposConsensusBlockData(mt, mb)

    List(1, 2).foreach { version =>
      val timestamp = System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L
      val block = Block.buildAndSign(version.toByte, timestamp, ByteStr(reference), cbd,
        tbd.map{tx: Transaction => ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)}, gen)
      val parsedBlock = Block.parseBytes(block.bytes).get
      assert(Signed.validateSignatures(block).isRight)
      assert(Signed.validateSignatures(parsedBlock).isRight)
      assert(parsedBlock.consensusData.mintTime == mt)
      assert(parsedBlock.consensusData.mintBalance == mb)
      assert(parsedBlock.version.toInt == version)
      assert(parsedBlock.signerData.generator.publicKey.sameElements(gen.publicKey))
      assert(parsedBlock.transactionData.size == 1)
      assert(parsedBlock.transactionData(0).status == TransactionStatus.Success)
      assert(parsedBlock.transactionData(0).feeCharged == 1000)
    }
  }
}
