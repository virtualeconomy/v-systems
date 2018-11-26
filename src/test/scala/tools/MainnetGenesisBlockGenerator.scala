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
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    154285740L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    263314330L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    360000060L * UnitsInVee,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
  )

  // add wallet address
  val wallet_addresses = Array (
    "ARKwwhnX2mk9V79kuvb3tEWVyri5Z2HFsPR",
    "AR2vo3jQjoyJLQysg99AYTR1SQ5mHqGhS1P",
    "AR8ejrETNWLaABp27fYEdh291MR1kDC92ue",
    "ARBdeGKBfd2aJd5BEBJz2npX55nPVa4Tn6V",
    "ARAA6uz8dthMQaNSr3K4UhQY279UrJHQ37x",
    "ARGsRvusZpKpt4XFdVHAiWNCKmwoCG7Fm3n",
    "AR8fEYgWobHthXWNgUvhNbHaV3npVwqssM3",
    "AR5uQhUb5pp2PFLC6CMHdXzrSstaw55EQFb",
    "ARErPEJhWFzsnVAMDczUkYZ1LCbYRkPmYLT",
    "AR8HGzodYPzsUHtx15hfP2dytwN8t88HoXz",
    "ARMVYz7Nw2gJUrfLoQYZns9QgYTGnUnup9q",
    "AR5i6EcHeTAXxyAFwRvaT3VFBZW34hohj1C",
    "ARMhBmbgkFBpXAeGR3JEC6ynjKC4d83K7rH",
    "AR8DGjK1xrSAq3JvbvemCydsrByd5NgJPZP",
    "ARDX6cr3hdqAVUGH57bdNzj13wyWnXcTLWa",
    "ARQXTpJAxSME8G7eUuRhJM8MFtuXZhU8TZv", // with non-zero balance
    "AREi5xZQkffJdXbLmgmESYJZAkrGJUu6qBV",
    "AR3d9ELYQkfQpHut4hURAP5FQZ88iRHUAto",
    "ARLnNUEHqJshHPmauToNytZuWvXH5ZQk8jX",
    "AR9GT7DZyLxvofVLsoYtcrDzMYX6Q2VvXVU",
    "AR4v6KX8eTLTa7hGrNMVJsPWfqxWKh6Mf7o",
    "AR3JVT7Z4BvAjdUf7Y593WB7pFnEuQQuFFj",
    "ARKSxgC7m8S5uLAtYusWYTykd6YyUHUA5Uj",
    "ARLa44HK7iuvfJ7yCc1Y6rUJVCS9KSuLfxa",
    "AR9fDBBdQzeGmQY2ZikX2yUZMEMyjKEC5EW",
    "ARAsvWqn71V93NzMm7C7k2zkRwnpMLDRTR5",
    "ARQcD8MVftg2HhbufU2RWB9cTtnVe4b6BqG",
    "ARRf8LUEayHeYpqzmMTj6QB9iH76gh75qkg",
    "ARQ4rDViLmPT7oEgEX6JRpA6qWQXhLypEYx",
    "ARCkTMPANUYYZudAHTnJUjUYfV3UMnSqYCC",
    "ARPnxBFbMzQQn4SncJ2WdH61ynqcPcninV4"//minters
  )

  val genesis_slots = Array (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56
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
