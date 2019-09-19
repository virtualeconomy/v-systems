package vsys.utils

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.state2.ByteStr
import scorex.account.{Address, AddressScheme, PrivateKeyAccount}
import scorex.block.Block
import scorex.transaction.TransactionParser.SignatureLength
import scorex.transaction.{GenesisTransaction, Transaction}
import vsys.consensus.spos.SposConsensusBlockData
import vsys.settings.{GenesisSettings, GenesisTransactionSettings}
import vsys.transaction.{ProcessedTransaction, TransactionStatus}
import vsys.wallet.Wallet

import scala.concurrent.duration._

object GenesisBlockGenerator {

  val genesisSigner = PrivateKeyAccount(Array.empty)
  val reference = ByteStr(Array.fill(SignatureLength)(-1: Byte))

  val file = new File("testnet_quick_start.conf")
  var cfg: Config = ConfigFactory.parseFile(file)
  loadConfig(cfg)

  val initial_balance: Long = cfg.getLong("vsys.wallet.initial-balance")
  var distributions = Map(
    1 -> Seq(initial_balance)
  )
  val miner_num: Int = cfg.getInt("vsys.miner.quorum")
  val peer_num: Int = cfg.getInt("vsys.peer.quorum")
  val interval: Double = Math.floor(60.0 / (peer_num+miner_num))
  val addresses: Config = cfg.getConfig("vsys.wallet.slots")
  // add test use wallet address
  var test_wallet_addresses: Array[String] = Array.empty[String]
  var last_sequence = Seq(initial_balance)
  for (miner_index <- 0 until miner_num + peer_num) {
    test_wallet_addresses :+= addresses.getConfig("slot" + (miner_index * interval).toString).getString("address")
    if (miner_index > 0) {
      var miner_balance: Long = (initial_balance * addresses.getConfig("slot" + (miner_index * interval).toString).getDouble("balance_distribution")).toLong
      last_sequence = last_sequence.updated(0, last_sequence(0) - miner_balance)
      last_sequence :+= miner_balance
      distributions += miner_index +1 -> last_sequence
    }
  }


  def generateFullAddressInfo(n: Int) = {

    println("n=" + n + ", address = " + test_wallet_addresses(n / interval))

    val seed = ByteStr(Array.fill(32)((scala.util.Random.nextInt(256)).toByte)).toString
    val acc = Wallet.generateNewAccount(seed, 0)
    val privateKey = ByteStr(acc.privateKey)
    val publicKey = ByteStr(acc.publicKey)
    // change address value for testnetwow
    //    val address = acc.toAddress
    val address = Address.fromString(test_wallet_addresses(n / interval)).right.get  //ByteStr(Base58.decode(test_wallet_addresses(n)).get)

    (seed, ByteStr(acc.seed), privateKey, publicKey, address)
  }

  def generate(networkByte: Char, accountsTotal: Int, mintTime: Long, averageBlockDelay: FiniteDuration) = {
    scorex.account.AddressScheme.current = new AddressScheme {
      override val chainId: Byte = networkByte.toByte
    }

    val timestamp = System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L
    val initialBalance = 1000000000000000000L

    val mt = if (mintTime < 0) timestamp / 10000000000L * 10000000000L else mintTime

    val accounts = Range(0, 60, interval).map(n => n -> generateFullAddressInfo(n))
    val genesisTxs = accounts.map { case (n, (_, _, _, _, address)) => GenesisTransaction(address, distributions(accountsTotal)(n / interval), n, timestamp, ByteStr.empty) }

    println(ByteStr(genesisTxs.head.bytes).base58)
    // set the genesisblock's minting Balance to 0
    val genesisBlock = Block.buildAndSign(1, timestamp, reference, SposConsensusBlockData(mt, 0L),
      genesisTxs.map{tx: Transaction => ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)}, genesisSigner)
    val signature = genesisBlock.signerData.signature

    (accounts, GenesisSettings(timestamp, timestamp, initialBalance, Some(signature),
      genesisTxs.map(tx => GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount, tx.slotId)), mt, averageBlockDelay))

  }

  def print(accs: Seq[(Int, (String, ByteStr, ByteStr, ByteStr, Address))], settings: GenesisSettings): Unit = {

    println("Addresses:")
    accs.foreach { case (n, (seed, accSeed, priv, pub, addess)) =>
      println(
        s"""($n):
           | seed: $seed
           | accSeed: $accSeed
           | priv: $priv
           | pub : $pub
           | addr: ${addess.address}
           |
       """.stripMargin)
    }
    println(
      s"""GenesisSettings:
         | timestamp: ${settings.timestamp}
         | blockTimestamp: ${settings.blockTimestamp}
         | averageBlockDelay: ${settings.averageBlockDelay}
         | initialBalance: ${settings.initialBalance}
         | initialMintTime: ${settings.initialMintTime}
         | signature: ${settings.signature}
         | transactions: ${settings.transactions.mkString("\n   ", "\n   ", "")}
     """.stripMargin)
  }

  def main(args: Array[String]): Unit = {
    val (a, s) = generate('T', miner_num + peer_num, -1, 60.seconds)
    print(a, s)
  }
}
