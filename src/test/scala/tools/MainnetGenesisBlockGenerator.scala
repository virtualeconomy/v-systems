package tools

import vee.settings.{GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.state2.ByteStr
import scorex.account.{Address, AddressScheme, PrivateKeyAccount}
import scorex.block.Block
import vee.consensus.spos.SposConsensusBlockData
import scorex.transaction.{GenesisTransaction, Transaction}
import vee.transaction.{TransactionStatus, ProcessedTransaction}
import scorex.transaction.TransactionParser.SignatureLength
import com.wavesplatform.settings.Constants._

import scala.concurrent.duration._

object MainnetGenesisBlockGenerator extends App {

  val genesisSigner = PrivateKeyAccount(Array.empty)
  val reference = ByteStr(Array.fill(SignatureLength)(-1: Byte))
  val balanceDistributions = Seq(360000060L * UnitsInVee,
    154285740L * UnitsInVee,
    250971470L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    123428592L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    390857208L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    263314330L * UnitsInVee,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
  )

  // add wallet address
  val wallet_addresses = Array (
    "ARMxPV8puShq49KzPoQb5XGttxghvLdMFg1",
    "ARFHdvqddPNfMXNt9fgH7Lw1aGRgL29wTKY",
    "ARQSe9A3DLryBqN4Y1NV1PhERnuWaDLheza",
    "ARHY8cocXeGnH3q4bkLpU56sCg28fF5hSWV",
    "ARCry9n7JvHF8rooXiEMhesAHP8zv3sBLDr",
    "AR7M83ZypZc5WRzdZHjhULnQ7xzoMM8udJt",
    "ARPdpZ9PT5kDbi2U6ZUgxoFwJzFchz7Fcsw",
    "ARCK84TwkqzQEksj8pQTxbsCuRY9YGsAbSU",
    "ARKbFz4N3f8Rgonf1Afd2aix2QtkCUjnWqW",
    "ARRo5K1rfLk9wjXhUjAqxdZjK7LTjx7Y7ki",
    "ARBLnj5P9XNspGRGj8UjS1Fny3pkvxJUqJV",
    "AR5c7ibqGhkyjzYu143E171TiFdmPFHofKb",
    "ARJukkvk47P9JKpdpjbvPQFr8Lv4D8Qsqnn",
    "ARBD7fqPGPq5stYKyk3EbZe1w1FDYhh2KnZ",
    "ARH6GEvr8FV12HN4rZ969K885sukp5qdNJY",
    "AR2pa1RW5jvKV4b7PNnZG7Az6emz5WtiE4r", // with non-zero balance
    "ARQcQXvUVeZ55cV7VWBzvTZCfGLNcQQ4BSm",
    "ARPbddZcV3gpXT4WBxVP44HAgroqeQw38kt",
    "AR73vwfW1sNALXCZTqbypNuEEbPTKccERok",
    "AR3yL8ALb4oLYqUruLMmi6DYcNiCrCCXXrH",
    "ARL8X83xFfU9aHXQoP6HW5kPzrCurLjoCT1",
    "AR2hWH9sugKjdArn7A9yYkUWQdGHjefcJq9",
    "AR8QbPBWx9x2yeWCWXdJVxftb7RQ6hqwAiU",
    "AR4SSjr2tuZxSmJpfwHCbxWEp2xQERh4JWL",
    "AR8n5DjETjUKFApuJNRhkWsTbWPeSYQorBS",
    "AR7mJnfqmEw5pwCyWkJRqdbcVhGhaycLaVs"  //minters
  )

  val genesis_slots = Array (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 4, 12, 16, 24, 28, 36, 40, 48, 52
  )

  def generate(networkByte: Char, averageBlockDelay: FiniteDuration) :(IndexedSeq[(Int, Address)], GenesisSettings) = {
    scorex.account.AddressScheme.current = new AddressScheme {
      override val chainId: Byte = networkByte.toByte
    }

    val timestamp = System.currentTimeMillis() * 1000000L + System.nanoTime() % 1000000L
    val initialBalance = balanceDistributions.reduceLeft(_ + _)

    val mt = timestamp/ 4L * 4L / 1000000000L * 1000000000L

    val accounts = wallet_addresses.indices.map(n => n -> Address.fromString(wallet_addresses(n)).right.get)
    val genesisTxs = accounts.map { case (n, address) => GenesisTransaction(address, balanceDistributions(n), genesis_slots(n), timestamp, ByteStr.empty) }

    // set the genesisblock's minting Balance to 0
    val genesisBlock = Block.buildAndSign(1, timestamp, reference, SposConsensusBlockData(mt, 0L),
      genesisTxs.map{tx: Transaction => ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)}, genesisSigner)
    val signature = genesisBlock.signerData.signature

    (accounts, GenesisSettings(timestamp, timestamp, initialBalance, Some(signature),
      genesisTxs.map(tx => GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount, tx.slotId)), mt, averageBlockDelay))

  }

  def print(accs: Seq[(Int, Address)], settings: GenesisSettings): Unit = {
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

  val (a, s) = generate('M', 60.seconds)
  print(a, s)

}
