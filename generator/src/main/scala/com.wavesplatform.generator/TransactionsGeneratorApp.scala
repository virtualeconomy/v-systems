package com.wavesplatform.generator

import com.wavesplatform.generator.GeneratorSettings._

import java.io.File
import java.net.{InetAddress, InetSocketAddress}

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes
import org.slf4j.LoggerFactory
import vsys.account.{AddressScheme, PrivateKeyAccount}
import vsys.utils.crypto.encode.Base58
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.TransactionParser.{TransactionType => TT}
import vsys.blockchain.transaction.assets.exchange.AssetPair
import vsys.utils.LoggerFacade

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TransactionsGeneratorApp extends App {
  val log = LoggerFacade(LoggerFactory.getLogger(getClass))

  val generatorConfig = fromConfig(readConfig(args.headOption))
  import generatorConfig._

  AddressScheme.current value_= new AddressScheme {
    override val chainId: Byte = generatorConfig.chainId.toByte
  }

  log.info(s"Generating $n transactions every $every from addresses:")
  accounts.foreach(a => log.info(a.address))
  val sender = new NetworkSender(sendTo, chainId, "generator", 38262732757L)
  sys.addShutdownHook(sender.close())
  while (true) {
    val txs = TransactionGenerator.gen(txProbabilities, accounts, n)
    txs.foreach(t => log.trace(t.toString))
    Await.result(
      sender.sendByNetwork(txs.map(tx => RawBytes(25.toByte, tx.bytes)): _*)
      .map(_ => log.info("Transactions was sent")), Duration.Inf)
    Thread.sleep(every.toMillis)
  }
}
