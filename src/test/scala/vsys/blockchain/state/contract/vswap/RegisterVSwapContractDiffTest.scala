package vsys.blockchain.state.contract.vswap

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.vswap.VSwapContractGen
import vsys.blockchain.transaction.TransactionGen

class RegisterVSwapContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VSwapContractGen {

}
