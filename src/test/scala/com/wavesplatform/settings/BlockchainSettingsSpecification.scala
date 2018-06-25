package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import com.wavesplatform.state2.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

import vee.settings.GenesisTransactionSettings

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  directory = "/waves"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = CUSTOM
        |    custom {
        |      address-scheme-character = "C"
        |      functionality {
        |        allow-temporary-negative-until = 1
        |        allow-invalid-payment-transactions-by-timestamp = 2
        |        require-sorted-transactions-after = 3
        |        generation-balance-depth-from-50-to-1000-after-height = 4
        |        minimal-generating-balance-after = 5
        |        allow-transactions-from-future-until = 6
        |        allow-unissued-assets-until = 7
        |        allow-burn-transaction-after = 8
        |        require-payment-unique-id-after = 9
        |        allow-lease-transaction-after = 10
        |        allow-exchange-transaction-after = 11
        |        allow-invalid-reissue-in-same-block-until-timestamp = 12
        |        allow-createalias-transaction-after = 13
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        allow-contend-slots-transaction-after = 16
        |        allow-leased-balance-transfer-until = 17
        |        allow-release-slots-transaction-after = 18
        |        num-of-slots = 5
        |        minting-speed = 5
        |      }
        |      genesis {
        |        timestamp = 1460678400000
        |        block-timestamp = 1460678400000
        |        signature = "BASE58BLKSGNATURE"
        |        initial-balance = 100000000000000
        |        initial-mint-time = 1529885280000000000
        |        average-block-delay = 60s
        |        transactions = [
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000001},
        |          {recipient = "BASE58ADDRESS2", amount = 49999999999999}
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/waves/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/waves/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/waves/data/checkpoint.dat")))
    //not snapshot
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(2)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(3)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(5)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(6)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(7)
    settings.functionalitySettings.allowBurnTransactionAfter should be(8)
    settings.functionalitySettings.requirePaymentUniqueIdAfter should be(9)
    settings.functionalitySettings.allowLeaseTransactionAfter should be(10)
    settings.functionalitySettings.allowExchangeTransactionAfter should be(11)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(12)
    settings.functionalitySettings.allowCreatealiasTransactionAfter should be(13)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(14)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(15)
    settings.functionalitySettings.allowContendSlotsTransactionAfter should be(16)
    settings.functionalitySettings.allowLeasedBalanceTransferUntil should be(17)
    settings.functionalitySettings.allowReleaseSlotsTransactionAfter should be(18)
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1460678400000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialMintTime should be(1529885280000000000L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L),
      GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L)))
  }

  it should "read testnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  directory = "/waves"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/waves/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/waves/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/waves/data/checkpoint.dat")))
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1477958400000L)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(1477958400000000000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1477958400000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(Long.MinValue)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(Long.MinValue)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1478100000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1479416400000L)
    settings.functionalitySettings.allowBurnTransactionAfter should be(1481110521000L)
    settings.functionalitySettings.requirePaymentUniqueIdAfter should be(1485942685000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1492560000000000000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1492560000000000000L)
    settings.functionalitySettings.allowExchangeTransactionAfter should be(1483228800000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(51500)
    settings.functionalitySettings.allowCreatealiasTransactionAfter should be(1493596800000L)
    settings.functionalitySettings.allowContendSlotsTransactionAfter should be(1493596800000L)
    settings.functionalitySettings.allowReleaseSlotsTransactionAfter should be(1493596800000L)
    settings.functionalitySettings.allowLeasedBalanceTransferUntil should be(1495238400000L)
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1529898440087220090L)
    settings.genesisSettings.timestamp should be(1529898440087220090L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("36uHjhrpSDuoC1zUkBsWyYKLwenTxykk5Bq2JcA3eBmop2anseJQDFBkAXWRgT828x8xyHVSQ4MuVCAqbKZt35ge").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)

    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("3N1YJ6RaYDkmh1fiy8ww7qCXDnySqyxceDS", 3000000000000000L),
      GenesisTransactionSettings("3NCorpZy4JhrtXtKeqLTft7Li79vehDssvr", 2000000000000000L),
      GenesisTransactionSettings("3MvRSHqRtn4sWgwr3EnDrP6VjphnQrrEB6t", 1500000000000000L),
      GenesisTransactionSettings("3MxPwccKXAp9bT9edNLRZHBvJhuEgrdJ61K", 500000000000000L),
      GenesisTransactionSettings("3MzgaPu93fkmCqgkPZHLHgGt3pUZapuU3jM", 600000000000000L),
      GenesisTransactionSettings("3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5", 600000000000000L),
      GenesisTransactionSettings("3MxYTgmMWiaKT82y4jfZaSPDqEDN1JbETvp", 600000000000000L),
      GenesisTransactionSettings("3MpZ718ivTCaRbra6JpABGV9Hdk75QAvpbj", 600000000000000L),
      GenesisTransactionSettings("3N3SZdKP5qWv7AsKXDC1Vk7unWg81oQ3ynK", 400000000000000L),
      GenesisTransactionSettings("3N15meHNxRzmfRYJJqrWA7p5NN2yd4CF62v", 200000000000000L)))
  }

  it should "read mainnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """waves {
        |  directory = "/waves"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/waves/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/waves/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/waves/data/checkpoint.dat")))
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('W')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1479168000000L)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(1479168000000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1479168000000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(232000L)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(1479168000000L)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1479168000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1479416400000L)
    settings.functionalitySettings.allowBurnTransactionAfter should be(1491192000000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1492768800000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1492768800000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(462000)
    settings.functionalitySettings.requirePaymentUniqueIdAfter should be(1491192000000L)
    settings.functionalitySettings.allowExchangeTransactionAfter should be(1491192000000L)
    settings.functionalitySettings.allowReleaseSlotsTransactionAfter should be(1503914400000L)
    settings.functionalitySettings.allowContendSlotsTransactionAfter should be(1503914400000L)
    settings.functionalitySettings.allowLeasedBalanceTransferUntil should be(Long.MaxValue)
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1465742577614L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", 9999999500000000L),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", 100000000L),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", 100000000L),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", 100000000L),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", 100000000L),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", 100000000L)))
  }
}
