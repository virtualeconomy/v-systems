package com.wavesplatform.it

import vsys.account.AddressScheme

trait IntegrationTestsScheme {
  AddressScheme.current value_= new AddressScheme {
    override val chainId: Byte = 'I'
  }
}
