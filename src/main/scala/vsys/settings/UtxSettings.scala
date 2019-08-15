package vsys.settings

import scala.concurrent.duration.FiniteDuration


case class UtxSettings(maxSize: Int, maxTransactionAge: FiniteDuration)
