package vsys.settings

import com.typesafe.config.ConfigFactory
import vsys.blockchain.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        |  directory = "/vsys"
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
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        allow-leased-balance-transfer-until = 17
        |        allow-contract-transaction-after-height = 0
        |        allow-deposit-withdraw-contract-after-height = 0
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
        |    state {
        |      tx-type-account-tx-ids = on
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    //not snapshot
    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.numOfSlots should be (5)
    settings.functionalitySettings.mintingSpeed should be (5)
    settings.functionalitySettings.allowContractTransactionAfterHeight should be (0)
    settings.functionalitySettings.allowDepositWithdrawContractAfterHeight should be (0)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1460678400000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialMintTime should be(1529885280000000000L)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(Seq(
      GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L, -1),
      GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L, -1)))
    settings.stateSettings.txTypeAccountTxIds should be (true)
  }

  it should "read testnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        |  directory = "/vsys"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = TESTNET
        |    state {
        |      tx-type-account-tx-ids = off
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.numOfSlots should be (60)
    settings.functionalitySettings.mintingSpeed should be (1)
    settings.functionalitySettings.allowContractTransactionAfterHeight should be (4236000) // same as the setting
    settings.functionalitySettings.allowDepositWithdrawContractAfterHeight should be (12550000)
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

    settings.stateSettings.txTypeAccountTxIds should be (false)
  }

  it should "read mainnet settings" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        |  directory = "/vsys"
        |  blockchain {
        |    minimum-in-memory-diff-blocks = 1
        |    type = MAINNET
        |    state {
        |      tx-type-account-tx-ids = off
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.minimumInMemoryDiffSize should be(1)
    settings.addressSchemeCharacter should be('M')
    settings.functionalitySettings.numOfSlots should be (60)
    settings.functionalitySettings.mintingSpeed should be (1)
    settings.functionalitySettings.allowContractTransactionAfterHeight should be (6100000) // same as the setting
    settings.functionalitySettings.allowDepositWithdrawContractAfterHeight should be (Int.MaxValue)
    settings.genesisSettings.blockTimestamp should be(1543286357457333127L)
    settings.genesisSettings.timestamp should be(1543286357457333127L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("3yYNd7quEWaWytrAug4yGwQvpL3PVJegf9d9NTv9PVE3ouBYJs5PTQqxCjd294uK1zPLj6G5Tk447LqFMWdSFvaQ").toOption)
    settings.genesisSettings.initialBalance should be(514285800000000000L) //changed the total initialBalance in default setting
    settings.genesisSettings.transactions should be(Seq(
        GenesisTransactionSettings("ARKwwhnX2mk9V79kuvb3tEWVyri5Z2HFsPR",36000006000000000L,-1),
        GenesisTransactionSettings("AR2vo3jQjoyJLQysg99AYTR1SQ5mHqGhS1P",15428574000000000L,-1),
        GenesisTransactionSettings("AR8ejrETNWLaABp27fYEdh291MR1kDC92ue",25097147000000000L,-1),
        GenesisTransactionSettings("ARBdeGKBfd2aJd5BEBJz2npX55nPVa4Tn6V",36000006000000000L,-1),
        GenesisTransactionSettings("ARAA6uz8dthMQaNSr3K4UhQY279UrJHQ37x",36000006000000000L,-1),
        GenesisTransactionSettings("ARGsRvusZpKpt4XFdVHAiWNCKmwoCG7Fm3n",36000006000000000L,-1),
        GenesisTransactionSettings("AR8fEYgWobHthXWNgUvhNbHaV3npVwqssM3",36000006000000000L,-1),
        GenesisTransactionSettings("AR5uQhUb5pp2PFLC6CMHdXzrSstaw55EQFb",15428574000000000L,-1),
        GenesisTransactionSettings("ARErPEJhWFzsnVAMDczUkYZ1LCbYRkPmYLT",36000006000000000L,-1),
        GenesisTransactionSettings("AR8HGzodYPzsUHtx15hfP2dytwN8t88HoXz",36000006000000000L,-1),
        GenesisTransactionSettings("ARMVYz7Nw2gJUrfLoQYZns9QgYTGnUnup9q",36000006000000000L,-1),
        GenesisTransactionSettings("AR5i6EcHeTAXxyAFwRvaT3VFBZW34hohj1C",26331433000000000L,-1),
        GenesisTransactionSettings("ARMhBmbgkFBpXAeGR3JEC6ynjKC4d83K7rH",36000006000000000L,-1),
        GenesisTransactionSettings("AR8DGjK1xrSAq3JvbvemCydsrByd5NgJPZP",36000006000000000L,-1),
        GenesisTransactionSettings("ARDX6cr3hdqAVUGH57bdNzj13wyWnXcTLWa",36000006000000000L,-1),
        GenesisTransactionSettings("ARQXTpJAxSME8G7eUuRhJM8MFtuXZhU8TZv",36000006000000000L,-1),
        GenesisTransactionSettings("AREi5xZQkffJdXbLmgmESYJZAkrGJUu6qBV",0L,0),
        GenesisTransactionSettings("AR3d9ELYQkfQpHut4hURAP5FQZ88iRHUAto",0L,4),
        GenesisTransactionSettings("ARLnNUEHqJshHPmauToNytZuWvXH5ZQk8jX",0L,8),
        GenesisTransactionSettings("AR9GT7DZyLxvofVLsoYtcrDzMYX6Q2VvXVU",0L,12),
        GenesisTransactionSettings("AR4v6KX8eTLTa7hGrNMVJsPWfqxWKh6Mf7o",0L,16),
        GenesisTransactionSettings("AR3JVT7Z4BvAjdUf7Y593WB7pFnEuQQuFFj",0L,20),
        GenesisTransactionSettings("ARKSxgC7m8S5uLAtYusWYTykd6YyUHUA5Uj",0L,24),
        GenesisTransactionSettings("ARLa44HK7iuvfJ7yCc1Y6rUJVCS9KSuLfxa",0L,28),
        GenesisTransactionSettings("AR9fDBBdQzeGmQY2ZikX2yUZMEMyjKEC5EW",0L,32),
        GenesisTransactionSettings("ARAsvWqn71V93NzMm7C7k2zkRwnpMLDRTR5",0L,36),
        GenesisTransactionSettings("ARQcD8MVftg2HhbufU2RWB9cTtnVe4b6BqG",0L,40),
        GenesisTransactionSettings("ARRf8LUEayHeYpqzmMTj6QB9iH76gh75qkg",0L,44),
        GenesisTransactionSettings("ARQ4rDViLmPT7oEgEX6JRpA6qWQXhLypEYx",0L,48),
        GenesisTransactionSettings("ARCkTMPANUYYZudAHTnJUjUYfV3UMnSqYCC",0L,52),
        GenesisTransactionSettings("ARPnxBFbMzQQn4SncJ2WdH61ynqcPcninV4",0L,56)))
    
    settings.stateSettings.txTypeAccountTxIds should be (false)
  }
}
