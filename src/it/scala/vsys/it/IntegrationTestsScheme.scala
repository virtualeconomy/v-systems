package vsys.it

import vsys.account.AddressScheme

trait IntegrationTestsScheme {
  AddressScheme.current.value = new AddressScheme {
    override val chainId: Byte = 'I'
  }
}
