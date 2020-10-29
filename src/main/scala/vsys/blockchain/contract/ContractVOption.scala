package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVOption {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(), // Triggers
    Seq(), // Functions
    stateVarSeq, // StateVars
    stateMapSeq, // StateMaps
    Seq()  // Textual
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "baseTokenId", "targetTokenId", "optionTokenId", "proofTokenId", "executeTime",
                          "executeDeadline", "optionStatus", "maxIssueNum", "reservedOption", "reservedProof",
                          "price", "priceUnit", "tokenLocked", "tokenCollected")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val baseTokenIdStateVar: StateVar         = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val targetTokenIdStateVar: StateVar       = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val optionTokenIdStateVar: StateVar       = StateVar(3.toByte, DataType.TokenId.id.toByte)
  val proofTokenIdStateVar: StateVar        = StateVar(4.toByte, DataType.TokenId.id.toByte)
  val executeTimeStateVar: StateVar         = StateVar(5.toByte, DataType.Timestamp.id.toByte)
  val executeDeadlineStateVar: StateVar     = StateVar(6.toByte, DataType.Timestamp.id.toByte)
  val optionStatusStateVar: StateVar        = StateVar(7.toByte, DataType.Boolean.id.toByte)
  val maxIssueNumStateVar: StateVar         = StateVar(8.toByte, DataType.Amount.id.toByte)
  val reservedOptionStateVar: StateVar      = StateVar(9.toByte, DataType.Amount.id.toByte)
  val reservedProofStateVar: StateVar       = StateVar(10.toByte, DataType.Amount.id.toByte)
  val priceStateVar: StateVar               = StateVar(11.toByte, DataType.Amount.id.toByte)
  val priceUnitStateVar: StateVar           = StateVar(12.toByte, DataType.Amount.id.toByte)
  val tokenLockedStateVar: StateVar         = StateVar(13.toByte, DataType.Amount.id.toByte)
  val tokenCollectedStateVar: StateVar      = StateVar(14.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, baseTokenIdStateVar.arr, targetTokenIdStateVar.arr,
                             optionTokenIdStateVar.arr, proofTokenIdStateVar.arr,
                             executeTimeStateVar.arr, executeDeadlineStateVar.arr, optionStatusStateVar.arr,
                             maxIssueNumStateVar.arr, reservedOptionStateVar.arr, reservedProofStateVar.arr,
                             priceStateVar.arr, priceUnitStateVar.arr, tokenLockedStateVar.arr, tokenCollectedStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapBaseTokenBalance         = List("baseTokenBalance", "userAddress", "balance")
  val stateMapTargetTokenBalance       = List("targetTokenBalance", "userAddress", "balance")
  val stateMapOptionTokenBalance       = List("optionTokenBalance", "userAddress", "balance")
  val stateMapProofTokenBalance        = List("proofTokenBalance", "userAddress", "balance")
  val baseTokenBalanceMap: StateMap    = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val targetTokenBalanceMap: StateMap  = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val optionTokenBalanceMap: StateMap  = StateMap(2.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val proofTokenBalanceMap: StateMap   = StateMap(3.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)

  lazy val stateMapSeq = Seq(baseTokenBalanceMap.arr, targetTokenBalanceMap.arr, optionTokenBalanceMap.arr, proofTokenBalanceMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapBaseTokenBalance, stateMapTargetTokenBalance,
                                                              stateMapOptionTokenBalance, stateMapProofTokenBalance))

  // Initialization Trigger

  // Deposit Trigger

  // WithDraw Trigger

  // Functions

  // Textual
}