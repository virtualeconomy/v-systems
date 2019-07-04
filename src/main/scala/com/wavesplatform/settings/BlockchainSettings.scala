package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import vsys.settings._

case class FunctionalitySettings(numOfSlots: Int,
                                 mintingSpeed: Int,
                                 allowContractTransactionAfterHeight: Int)

object FunctionalitySettings {
  val MAINNET = FunctionalitySettings(
    numOfSlots = 60,
    mintingSpeed = 1,
    //TODO
    //set the value
    allowContractTransactionAfterHeight = Int.MaxValue)

  val TESTNET = FunctionalitySettings(
    numOfSlots = 60,
    mintingSpeed = 1,
    //TODO
    //set the value
    allowContractTransactionAfterHeight = 4236000)

  val configPath = "vsys.blockchain.custom.functionality"
}

case class StateSettings(txTypeAccountTxIds: Boolean)

object StateSettings {
  val configPath = "vsys.blockchain.state"
}

case class BlockchainSettings(addressSchemeCharacter: Char,
                              minimumInMemoryDiffSize: Int,
                              functionalitySettings: FunctionalitySettings,
                              genesisSettings: GenesisSettings,
                              stateSettings: StateSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "vsys.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('M', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings = config.as[FunctionalitySettings](s"$configPath.custom.functionality")
        val genesisSettings = config.as[GenesisSettings](s"$configPath.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      minimumInMemoryDiffSize = config.as[Int](s"$configPath.minimum-in-memory-diff-blocks"),
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings,
      stateSettings = config.as[StateSettings](s"$configPath.state"))
  }
}
