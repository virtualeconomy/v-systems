package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq()
  ).explicitGet()

  // State Var

  // State Map

  // Initialization Trigger

  // Deposit Trigger

  // WithDraw Trigger

  // Functions

  // Textual

}