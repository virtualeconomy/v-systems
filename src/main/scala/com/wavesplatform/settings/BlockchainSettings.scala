package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import vee.settings._

case class FunctionalitySettings(allowTemporaryNegativeUntil: Long,
                                 allowInvalidPaymentTransactionsByTimestamp: Long,
                                 requireSortedTransactionsAfter: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Long,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowBurnTransactionAfter: Long,
                                 allowLeaseTransactionAfter: Long,
                                 requirePaymentUniqueIdAfter: Long,
                                 allowExchangeTransactionAfter: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowCreatealiasTransactionAfter: Long,
                                 allowContendSlotsTransactionAfter: Long,
                                 allowReleaseSlotsTransactionAfter: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Long,
                                 allowLeasedBalanceTransferUntil: Long,
                                 numOfSlots: Int,
                                 mintingSpeed: Int)

object FunctionalitySettings {
  //TODO, change the default settings
  val MAINNET = FunctionalitySettings(allowTemporaryNegativeUntil = 1479168000000L,
    allowInvalidPaymentTransactionsByTimestamp = 1479168000000L,
    requireSortedTransactionsAfter = 1479168000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 232000L,
    minimalGeneratingBalanceAfter = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfter = 1491192000000L,
    allowLeaseTransactionAfter = 1491192000000L,
    requirePaymentUniqueIdAfter = 1491192000000L,
    allowExchangeTransactionAfter = 1491192000000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492768800000L,
    allowCreatealiasTransactionAfter = 1503914400000L, // 2017-08-28T10:00:00Z
    allowContendSlotsTransactionAfter = 1503914400000L,
    allowReleaseSlotsTransactionAfter = 1503914400000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    allowLeasedBalanceTransferUntil = Long.MaxValue,
    numOfSlots = 5,
    mintingSpeed = 5)

  val TESTNET = FunctionalitySettings(
    allowTemporaryNegativeUntil = 1477958400000L,
    allowInvalidPaymentTransactionsByTimestamp = 1477958400000000000L, // timestamp rescale
    requireSortedTransactionsAfter = 1477958400000L,
    generationBalanceDepthFrom50To1000AfterHeight = Long.MinValue,
    minimalGeneratingBalanceAfter = Long.MinValue,
    allowTransactionsFromFutureUntil = 1478100000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowBurnTransactionAfter = 1481110521000L,
    allowLeaseTransactionAfter = Long.MinValue,
    requirePaymentUniqueIdAfter = 1485942685000L,
    allowExchangeTransactionAfter = 1483228800000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492560000000000000L,
    allowCreatealiasTransactionAfter = 1493596800000L,
    allowContendSlotsTransactionAfter = 1493596800000L,
    allowReleaseSlotsTransactionAfter = 1493596800000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492560000000000000L,
    resetEffectiveBalancesAtHeight = 51500,
    allowLeasedBalanceTransferUntil = 1495238400000L,
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
