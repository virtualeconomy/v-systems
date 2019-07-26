package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class RestAPISettingsSpecification extends FlatSpec with Matchers {
  "RestAPISettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """
        |vsys {
        |  rest-api {
        |    enable: yes
        |    bind-address: "127.0.0.1"
        |    port: 9922
        |    api-key-hash: "BASE58APIKEYHASH"
        |    cors: yes
        |    custom-api {
        |      # Routes with "/transactions"
        |      transactions {
        |        address-transaction-count = off
        |        address-transaction-list = on
        |      }
        |  
        |      # Routes with "/addresses"'
        |      addresses {
        |      }
        |  
        |      # Routes with "/node"
        |      node {
        |      }
        |  
        |      # Routes with "/blocks"
        |      blocks {
        |      }
        |  
        |      # Routes with "/utils"
        |      utils {
        |      }
        |  
        |      # Routes with "/contracts"
        |      contracts {
        |      }
        |    }
        |  }
        |}
      """.stripMargin)
    val settings = RestAPISettings.fromConfig(config)

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(9922)
    settings.apiKeyHash should be ("BASE58APIKEYHASH")
    settings.cors should be(true)
    settings.customApiSettings.transactionsApiSettings.addressTransactionCount should be(false)
    settings.customApiSettings.transactionsApiSettings.addressTransactionList should be(true)
  }

}
