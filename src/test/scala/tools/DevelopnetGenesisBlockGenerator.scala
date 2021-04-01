package tools

import vsys.settings.{GenesisSettings, GenesisTransactionSettings}
import vsys.blockchain.state.ByteStr
import vsys.account.{Address, AddressScheme, PrivateKeyAccount}
import vsys.blockchain.block.Block
import vsys.blockchain.block.SposConsensusBlockData
import vsys.blockchain.transaction.{TransactionStatus, ProcessedTransaction, GenesisTransaction, Transaction}
import vsys.blockchain.transaction.TransactionParser.SignatureLength
import vsys.settings.Constants._

import scala.concurrent.duration._

object DevelopnetGenesisBlockGenerator extends App {

  val genesisSigner = PrivateKeyAccount(Array.empty)
  val reference = ByteStr(Array.fill(SignatureLength)(-1: Byte))
  val balanceDistributions = Seq(
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys,
    500000000L * UnitsInVsys
  )

  // add wallet address
  val wallet_addresses = Array (
    "AU7J7Ct47rHDHDHCPQbvJU7DMoDKotim6RH",
    "AUC1pX6gpbbXizQHhiK29N65SWTWnsdYHrF",
    "AU9nfm3SibRot5p9nNgRdCprh5DfUQcecbE",
    "ATxyYEXUDWBck7F3mC2eH1cVqByi6NpuLBB",
    "AUB2Udm9SmgZ9Q4XeN2CeDtipc3CcjSo6CE",
    "AU6wmkLhY6xLSLS4yPx8nU8fUFpXC9F52wW",
    "AU8z8YCPsVEBFt6PEC7DVuiUxhpktXEJ2Ub",
    "AU9BVtUjeguQwY4CkFJAnoWBdnCYr7zjUzU",
    "ATrYRZXFZBhNiPZBUGtm5aLKc91dPeiobM9",
    "AU1ABmcdyGi5dXsyvgNATjmJffowwpfL9pu"

  )

  val genesis_slots = Array (
    0, 6, 12, 18, 24, 30, 36, 42, 48, 54
  )

  def generate(networkByte: Char, averageBlockDelay: FiniteDuration) :(IndexedSeq[(Int, Address)], GenesisSettings) = {
    vsys.account.AddressScheme.current = new AddressScheme {
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

  val (a, s) = generate('T', 60.seconds)
  print(a, s)

}
