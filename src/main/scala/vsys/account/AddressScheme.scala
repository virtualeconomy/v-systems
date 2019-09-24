package vsys.account

import scala.util.DynamicVariable

abstract class AddressScheme {
  val chainId: Byte
}

object AddressScheme {
  val current: DynamicVariable[AddressScheme] = new DynamicVariable(DefaultAddressScheme)
}

object DefaultAddressScheme extends AddressScheme {
  val chainId: Byte = 'T'.toByte
}
