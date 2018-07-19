package tools

import vee.settings.{GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.state2.ByteStr
import scorex.account.{Address, AddressScheme, PrivateKeyAccount}
import scorex.block.Block
import vee.consensus.spos.SposConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction.GenesisTransaction
import scorex.transaction.TransactionParser.SignatureLength
import vee.wallet.Wallet
import scala.concurrent.duration._

object GenesisBlockGenerator extends App {

  val genesisSigner = PrivateKeyAccount(Array.empty)
  val reference = ByteStr(Array.fill(SignatureLength)(-1: Byte))
  val distributions = Map(
    1 -> Seq(1000000000000000L),
    2 -> Seq(800000000000000L, 200000000000000L),
    3 -> Seq(650000000000000L, 200000000000000L, 150000000000000L),
    4 -> Seq(600000000000000L, 200000000000000L, 150000000000000L, 50000000000000L),
    5 -> Seq(480000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 120000000000000L),
    6 -> Seq(300000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 120000000000000L, 180000000000000L),
    7 -> Seq(300000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 120000000000000L, 120000000000000L, 60000000000000L),
    8 -> Seq(300000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 120000000000000L, 60000000000000L, 60000000000000L, 60000000000000L),
    9 -> Seq(300000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 60000000000000L, 60000000000000L, 60000000000000L, 60000000000000L, 60000000000000L),
    10 -> Seq(300000000000000L, 200000000000000L, 150000000000000L, 50000000000000L, 60000000000000L, 60000000000000L, 60000000000000L, 60000000000000L, 40000000000000L, 20000000000000L)
  )

  // add test use wallet address
  val test_wallet_addresses = Array (
      "3N1YJ6RaYDkmh1fiy8ww7qCXDnySqyxceDS",
      "3NCorpZy4JhrtXtKeqLTft7Li79vehDssvr",
      "3MvRSHqRtn4sWgwr3EnDrP6VjphnQrrEB6t",
      "3MxPwccKXAp9bT9edNLRZHBvJhuEgrdJ61K",
      "3MzgaPu93fkmCqgkPZHLHgGt3pUZapuU3jM",
      "3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5",
      "3MxYTgmMWiaKT82y4jfZaSPDqEDN1JbETvp",
      "3MpZ718ivTCaRbra6JpABGV9Hdk75QAvpbj",
      "3N3SZdKP5qWv7AsKXDC1Vk7unWg81oQ3ynK",
      "3N15meHNxRzmfRYJJqrWA7p5NN2yd4CF62v",
  )

  def generateFullAddressInfo(n: Int) = {
    println("n=" + n + ", address = " + test_wallet_addresses(n))

    val seed = Array.fill(32)((scala.util.Random.nextInt(256)).toByte)
    val acc = Wallet.generateNewAccount(seed, 0)
    val privateKey = ByteStr(acc.privateKey)
    val publicKey = ByteStr(acc.publicKey)
    // change address value for testnet
    //    val address = acc.toAddress
    val address = Address.fromString(test_wallet_addresses(n)).right.get  //ByteStr(Base58.decode(test_wallet_addresses(n)).get)

    (ByteStr(seed), ByteStr(acc.seed), privateKey, publicKey, address)
  }

  def generate(networkByte: Char, accountsTotal: Int, mintTime: Long, averageBlockDelay: FiniteDuration) = {
    scorex.account.AddressScheme.current = new AddressScheme {
      override val chainId: Byte = networkByte.toByte
    }

    val timestamp = System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L
    val initialBalance = 1000000000000000L

    val mt = if (mintTime < 0) timestamp / 10000000000L * 10000000000L else mintTime

    val accounts = Range(0, accountsTotal).map(n => n -> generateFullAddressInfo(n))
    val genesisTxs = accounts.map { case (n, (_, _, _, _, address)) => GenesisTransaction(address, distributions(accountsTotal)(n), timestamp, ByteStr.empty) }
    // set the genesisblock's minting Balance to 0
    val genesisBlock = Block.buildAndSign(1, timestamp, reference, SposConsensusBlockData(mt, 0L, Array.fill(DigestSize)(0: Byte)), genesisTxs, genesisSigner)
    val signature = genesisBlock.signerData.signature

    (accounts, GenesisSettings(timestamp, timestamp, initialBalance, Some(signature),
      genesisTxs.map(tx => GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)), mt, averageBlockDelay))

  }

  def print(accs: Seq[(Int, (ByteStr, ByteStr, ByteStr, ByteStr, Address))], settings: GenesisSettings): Unit = {

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

  val (a, s) = generate('T', 10, -1, 60.seconds)
  print(a, s)


}
