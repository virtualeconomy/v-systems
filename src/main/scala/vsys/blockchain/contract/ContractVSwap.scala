package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(), // Triggers
    Seq(), // Functions
    stateVarSeq, // StateVars
    Seq(), // StateMaps
    Seq()  // Textual
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "tokenAId", "tokenBId", "tokenLiquidityId", "swapStatus", "minimumLiquidity",
                          "tokenAReserved", "tokenBReserved", "totalSupply", "liquidityTokenLeft")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenAIdStateVar: StateVar            = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val tokeBIdStateVar: StateVar             = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val tokenLiquidityIdStateVar: StateVar    = StateVar(3.toByte, DataType.TokenId.id.toByte)
  val swapStatusStateVar: StateVar          = StateVar(4.toByte, DataType.Boolean.id.toByte)
  val minimumLiquidityStateVar: StateVar    = StateVar(5.toByte, DataType.Amount.id.toByte)
  val tokenAReservedStateVar: StateVar      = StateVar(6.toByte, DataType.Amount.id.toByte)
  val tokenBReservedStateVar: StateVar      = StateVar(7.toByte, DataType.Amount.id.toByte)
  val totalSupplyStateVar: StateVar         = StateVar(8.toByte, DataType.Amount.id.toByte)
  val liquidityTokenLeftStateVar: StateVar  = StateVar(9.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, tokenAIdStateVar.arr, tokeBIdStateVar.arr, tokenLiquidityIdStateVar.arr,
                             swapStatusStateVar.arr, minimumLiquidityStateVar.arr, tokenAReservedStateVar.arr,
                             tokenBReservedStateVar.arr, totalSupplyStateVar.arr, liquidityTokenLeftStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map

  // Initialization Trigger

  // Deposit Trigger

  // WithDraw Trigger

  // Functions

  // Textual

}