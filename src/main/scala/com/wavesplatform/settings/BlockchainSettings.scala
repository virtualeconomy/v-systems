package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import vee.settings._

case class FunctionalitySettings(numOfSlots: Int,
                                 mintingSpeed: Int)

object FunctionalitySettings {
  //TODO, change the default settings
  val MAINNET = FunctionalitySettings(
    numOfSlots = 5,
    mintingSpeed = 5)

  val TESTNET = FunctionalitySettings(
    numOfSlots = 5,
    mintingSpeed = 5)

  val configPath = "vee.blockchain.custom.functionality"
}

case class BlockchainSettings(blockchainFile: Option[File],
                              stateFile: Option[File],
                              checkpointFile: Option[File],
                              addressSchemeCharacter: Char,
                              minimumInMemoryDiffSize: Int,
                              functionalitySettings: FunctionalitySettings,
                              genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "vee.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('M', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings = config.as[FunctionalitySettings]("vee.blockchain.custom.functionality")
        val genesisSettings = config.as[GenesisSettings]("vee.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      blockchainFile = config.getAs[File](s"$configPath.blockchain-file"),
      stateFile = config.getAs[File](s"$configPath.state-file"),
      checkpointFile = config.getAs[File](s"$configPath.checkpoint-file"),
      addressSchemeCharacter = addressSchemeCharacter,
      minimumInMemoryDiffSize = config.as[Int](s"$configPath.minimum-in-memory-diff-blocks"),
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings)
  }
}
