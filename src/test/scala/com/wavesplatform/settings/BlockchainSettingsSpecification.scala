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
    settings.functionalitySettings.numOfSlots should be (60)
    settings.functionalitySettings.mintingSpeed should be (1)
    settings.genesisSettings.blockTimestamp should be(1535356447650226656L)
    settings.genesisSettings.timestamp should be(1535356447650226656L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("5n4ewwZh9F4MMpSvtdxLCu5MUKnhEyUth2w3zEfpuiX3vwS1STNCdi51fmowJuLT1CfFg1DuodSvxwBZDANvGNej").toOption)
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
    settings.functionalitySettings.numOfSlots should be (60)
    settings.functionalitySettings.mintingSpeed should be (1)
    settings.genesisSettings.blockTimestamp should be(1537343925754686951L)
    settings.genesisSettings.timestamp should be(1537343925754686951L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("2XrcU7AoEbs1qACVgo6BADf3hbjPWtSaG2L36RVJAUmFvL96dWMrm6ohGb69uFi5CiCKNPaEBc9r6UYPbk5mTGwg").toOption)
    settings.genesisSettings.initialBalance should be(514285800000000000L) //changed the total initialBalance in default setting
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("ARMxPV8puShq49KzPoQb5XGttxghvLdMFg1",36000006000000000L,-1),
      GenesisTransactionSettings("ARFHdvqddPNfMXNt9fgH7Lw1aGRgL29wTKY",15428574000000000L,-1),
      GenesisTransactionSettings("ARQSe9A3DLryBqN4Y1NV1PhERnuWaDLheza",25097147000000000L,-1),
      GenesisTransactionSettings("ARHY8cocXeGnH3q4bkLpU56sCg28fF5hSWV",36000006000000000L,-1),
      GenesisTransactionSettings("ARCry9n7JvHF8rooXiEMhesAHP8zv3sBLDr",36000006000000000L,-1),
      GenesisTransactionSettings("AR7M83ZypZc5WRzdZHjhULnQ7xzoMM8udJt",12342859200000000L,-1),
      GenesisTransactionSettings("ARPdpZ9PT5kDbi2U6ZUgxoFwJzFchz7Fcsw",36000006000000000L,-1),
      GenesisTransactionSettings("ARCK84TwkqzQEksj8pQTxbsCuRY9YGsAbSU",36000006000000000L,-1),
      GenesisTransactionSettings("ARKbFz4N3f8Rgonf1Afd2aix2QtkCUjnWqW",39085720800000000L,-1),
      GenesisTransactionSettings("ARRo5K1rfLk9wjXhUjAqxdZjK7LTjx7Y7ki",36000006000000000L,-1),
      GenesisTransactionSettings("ARBLnj5P9XNspGRGj8UjS1Fny3pkvxJUqJV",36000006000000000L,-1),
      GenesisTransactionSettings("AR5c7ibqGhkyjzYu143E171TiFdmPFHofKb",36000006000000000L,-1),
      GenesisTransactionSettings("ARJukkvk47P9JKpdpjbvPQFr8Lv4D8Qsqnn",36000006000000000L,-1),
      GenesisTransactionSettings("ARBD7fqPGPq5stYKyk3EbZe1w1FDYhh2KnZ",36000006000000000L,-1),
      GenesisTransactionSettings("ARH6GEvr8FV12HN4rZ969K885sukp5qdNJY",36000006000000000L,-1),
      GenesisTransactionSettings("AR2pa1RW5jvKV4b7PNnZG7Az6emz5WtiE4r",26331433000000000L,-1),
      GenesisTransactionSettings("ARKMDAveps99s5k9ESiXjcXM9Ddp8p3GGAm",0,0),
      GenesisTransactionSettings("ARKM1PkMevWdBjyEeMPAb7vrWWsJptDj891",0,4),
      GenesisTransactionSettings("ARCmPJxYBkYAtxnu8qpoLA2iQbf7aJFRX2c",0,12),
      GenesisTransactionSettings("ARNP2wJbehKh5Ck4Z2iR4pmnWiwgdsYh6Fw",0,16),
      GenesisTransactionSettings("ARQiW9ckRc1GbbuEN3h4s18H8YH8mke15qB",0,24),
      GenesisTransactionSettings("ARMfFAXxGZoFpQ7BwSKqjz9u2nxAtBRYXQ5",0,28),
      GenesisTransactionSettings("ARM1o54Hdo3hHy83NE4BcmJHEdVUdeULZyZ",0,36),
      GenesisTransactionSettings("ARDVLTDWf9Hwe3PBk16SNW7HCZXJPjJ43qE",0,40),
      GenesisTransactionSettings("ARNNKhgFhW8ctmQYvckgf81PyHjsxDEzySd",0,48),
      GenesisTransactionSettings("AR87ef8yc2Ldt1FGrJi85HW479TVeBCgS5V",0,52)))
  }
}
