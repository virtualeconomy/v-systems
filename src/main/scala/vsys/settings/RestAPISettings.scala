package vsys.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class CustomTransactionsAPISettings(addressTransactionCount: Boolean,
                                         addressTransactionList: Boolean)

object CustomTransactionsAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.transactions"
}

case  class CustomAddressesAPISettings()

object CustomAddressesAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.addresses"
}

case class CustomNodeAPISettings()

object CustomNodeAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.node"
}

case class CustomBlocksAPISettings()

object CustomBlocksAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.blocks"
}

case class CustomUtilsAPISettings()

object CustomUtilsAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.utils"
}

case class CustomContractsAPISettings()

object CustomContractsAPISettings {
  val configPath: String = "vsys.rest-api.custom-api.contracts"
}

case class CustomAPISettings(transactionsApiSettings: CustomTransactionsAPISettings,
                             addressesApiSettings: CustomAddressesAPISettings,
                             nodeApiSettings: CustomNodeAPISettings,
                             blocksApiSettings: CustomBlocksAPISettings,
                             utilsApiSettings: CustomUtilsAPISettings,
                             contractsApiSettings: CustomContractsAPISettings)

object CustomAPISettings {
  val configPath: String = "vsys.rest-api.custom-api"

  def fromConfig(config: Config): CustomAPISettings = {
    CustomAPISettings(
      transactionsApiSettings = config.as[CustomTransactionsAPISettings](CustomTransactionsAPISettings.configPath),
      addressesApiSettings = config.as[CustomAddressesAPISettings](CustomAddressesAPISettings.configPath),
      nodeApiSettings = config.as[CustomNodeAPISettings](CustomNodeAPISettings.configPath),
      blocksApiSettings = config.as[CustomBlocksAPISettings](CustomBlocksAPISettings.configPath),
      utilsApiSettings = config.as[CustomUtilsAPISettings](CustomUtilsAPISettings.configPath),
      contractsApiSettings = config.as[CustomContractsAPISettings](CustomContractsAPISettings.configPath)
    )
  }
}

case class RestAPISettings(enable: Boolean, bindAddress: String, port: Int, apiKeyHash: String, cors: Boolean, customApiSettings: CustomAPISettings)

object RestAPISettings {
  val configPath: String = "vsys.rest-api"

  def fromConfig(config: Config): RestAPISettings = {
    val enable = config.as[Boolean](s"$configPath.enable")
    val bindAddress = config.as[String](s"$configPath.bind-address")
    val port = config.as[Int](s"$configPath.port")
    val apiKeyHash = config.as[String](s"$configPath.api-key-hash")
    val cors = config.as[Boolean](s"$configPath.cors")
    val customApi = CustomAPISettings.fromConfig(config)

    RestAPISettings(enable, bindAddress, port, apiKeyHash, cors, customApi)
  }
}