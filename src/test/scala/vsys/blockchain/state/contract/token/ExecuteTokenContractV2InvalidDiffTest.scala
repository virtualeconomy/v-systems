package vsys.blockchain.state.contract.token

import org.scalacheck.Shrink
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.blockchain.contract.token.TokenContractV2Gen
import vsys.blockchain.transaction.TransactionGen

class ExecuteTokenContractV2InvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  
}
