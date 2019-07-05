package vsys.api.http

import com.typesafe.config.ConfigFactory
import vsys.settings.RestAPISettings
import scorex.crypto.encode.Base58
import vsys.utils.crypto.hash.SecureCryptographicHash

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"
  lazy val restAPISettings = {
    val keyHash = Base58.encode(SecureCryptographicHash(apiKey))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"vsys.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}
