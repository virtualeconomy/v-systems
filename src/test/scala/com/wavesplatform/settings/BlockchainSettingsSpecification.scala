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
      """vee {
        |  directory = "/vee"
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
        |        allow-lease-transaction-after = 10
        |        allow-exchange-transaction-after = 11
        |        allow-invalid-reissue-in-same-block-until-timestamp = 12
        |        allow-createalias-transaction-after = 13
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        allow-leased-balance-transfer-until = 17
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
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000001, slot-id = -1},
        |          {recipient = "BASE58ADDRESS2", amount = 49999999999999, slot-id = -1}
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/vee/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/vee/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/vee/data/checkpoint.dat")))
    //not snapshot
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1460678400000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialMintTime should be(1529885280000000000L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L, -1),
      GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L, -1)))
  }

  it should "read testnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vee {
        |  directory = "/vee"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/vee/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/vee/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/vee/data/checkpoint.dat")))
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1535078012713428395L)
    settings.genesisSettings.timestamp should be(1535078012713428395L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("41ay4EKgGwqhQcJ4beAX8JDpWBsKV3iMp3bka3k412oP1JAGUiizWVFtjmk7wwN9bYJXCS9h23jrLK9w4EmENoSG").toOption)
    settings.genesisSettings.initialBalance should be(1000000000000000000L)

    settings.genesisSettings.transactions should be(Seq(
        GenesisTransactionSettings("ATxpELPa3yhE5h4XELxtPrW9TfXPrmYE7ze",300000000000000000L, 0),
        GenesisTransactionSettings("ATtRykARbyJS1RwNsA6Rn1Um3S7FuVSovHK",200000000000000000L, 1),
        GenesisTransactionSettings("ATtchuwHVQmNTsRA8ba19juGK9m1gNsUS1V",150000000000000000L, 2),
        GenesisTransactionSettings("AU4AoB2WzeXiJvgDhCZmr6B7uDqAzGymG3L",50000000000000000L, 3),
        GenesisTransactionSettings("AUBHchRBY4mVNktgCgJdGNcYbwvmzPKgBgN",60000000000000000L, 4),
        GenesisTransactionSettings("AU6qstXoazCHDK5dmuCqEnnTWgTqRugHwzm",60000000000000000L, 5),
        GenesisTransactionSettings("AU9HYFXuPZPbFVw8vmp7mFmHb7qiaMmgEYE",60000000000000000L, 6),
        GenesisTransactionSettings("AUBLPMpHVV74fHQD8D6KosA76nusw4FqRr1",60000000000000000L, 7),
        GenesisTransactionSettings("AUBbpPbymsrM8QiXqS3NU7CrD1vy1EyonCa",40000000000000000L, 8),
        GenesisTransactionSettings("AU7nJLcT1mThXGTT1KDkoAtfPzc82Sgay1V",20000000000000000L, 9)
    ))
  }

  it should "read mainnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vee {
        |  directory = "/vee"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.blockchainFile should be(Some(new File("/vee/data/blockchain.dat")))
    settings.stateFile should be(Some(new File("/vee/data/state.dat")))
    settings.checkpointFile should be(Some(new File("/vee/data/checkpoint.dat")))
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('M')
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1465742577614L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption)
    settings.genesisSettings.initialBalance should be(1000000000000000000L) //changed the total initialBalance in default setting
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", 999999999500000000L, 0),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", 100000000L, 1),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", 100000000L, 2),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", 100000000L, 3),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", 100000000L, 4),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", 100000000L,5)))
  }
}
