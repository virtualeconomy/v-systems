package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen.StateVar
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVEscrow {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq()
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "judge", "tokenId", "duration", "judgeDuration")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val judgeStateVar: StateVar               = StateVar(1.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar: StateVar             = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val durationStateVar: StateVar            = StateVar(3.toByte, DataType.Timestamp.id.toByte)
  val judgeDurationStateVar: StateVar       = StateVar(4.toByte, DataType.Timestamp.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, judgeStateVar.arr, tokenIdStateVar.arr,
                             durationStateVar.arr, judgeDurationStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map

  // Initialization Trigger

  // Deposit Trigger

  // WithDraw Trigger

  // Functions

  // Textual
}