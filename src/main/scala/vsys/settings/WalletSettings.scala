package vsys.settings

import java.io.File

case class WalletSettings(file: Option[File], password: String, seed: Option[String])
