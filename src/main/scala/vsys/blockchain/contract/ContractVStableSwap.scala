package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVStableSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), // Triggers
    Seq(), // Functions
    stateVarSeq, // StateVars
    stateMapSeq,  // StateMaps
    Seq()  // Textual
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "baseTokenId", "targetTokenId", "maxOrderPerUser", "unitPriceBase", "unitPriceTarget")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val baseTokenIdStateVar: StateVar         = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val targetTokenIdStateVar: StateVar       = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val maxOrderPerUserStateVar: StateVar     = StateVar(3.toByte, DataType.Amount.id.toByte)
  val unitPriceBaseStateVar: StateVar       = StateVar(4.toByte, DataType.Amount.id.toByte)
  val unitPriceTargetStateVar: StateVar     = StateVar(5.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, baseTokenIdStateVar.arr, targetTokenIdStateVar.arr, maxOrderPerUserStateVar.arr,
                             unitPriceBaseStateVar.arr, unitPriceTargetStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapBaseTokenBalance         = List("baseTokenBalance", "userAddress", "balance")
  val stateMapTargetTokenBalance       = List("targetTokenBalance", "userAddress", "balance")
  val stateMapUserOrders               = List("userOrders", "userAddress", "orderNum")
  val stateMapFeeBase                  = List("feeBase", "orderId", "fee")
  val stateMapFeeTarget                = List("feeTarget", "orderId", "fee")
  val stateMapMinBase                  = List("minBase", "orderId", "amount")
  val stateMapMaxBase                  = List("maxBase", "orderId", "amount")
  val stateMapMinTarget                = List("minTarget", "orderId", "amount")
  val stateMapMaxTarget                = List("maxTarget", "orderId", "amount")
  val stateMapPriceBase                = List("priceBase", "orderId", "price")
  val stateMapPriceTarget              = List("priceTarget", "orderId", "price")
  val baseTokenBalanceMap: StateMap    = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val targetTokenBalanceMap: StateMap  = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val userOrdersMap: StateMap          = StateMap(2.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val feeBaseMap: StateMap             = StateMap(3.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val feeTargetMap: StateMap           = StateMap(4.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val minBaseMap: StateMap             = StateMap(5.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val maxBaseMap: StateMap             = StateMap(6.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val minTargetMap: StateMap           = StateMap(7.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val maxTargetMap: StateMap           = StateMap(8.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val priceBaseMap: StateMap           = StateMap(9.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val priceTargetMap: StateMap         = StateMap(10.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)

  lazy val stateMapSeq = Seq(baseTokenBalanceMap.arr, targetTokenBalanceMap.arr, userOrdersMap.arr,
                             feeBaseMap.arr, feeTargetMap.arr, minBaseMap.arr, maxBaseMap.arr,
                             minTargetMap.arr, maxTargetMap.arr, priceBaseMap.arr, priceTargetMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(
    Seq(stateMapBaseTokenBalance, stateMapTargetTokenBalance, stateMapUserOrders, stateMapFeeBase, stateMapFeeTarget,
        stateMapMinBase, stateMapMaxBase, stateMapMinTarget, stateMapMaxTarget, stateMapPriceBase, stateMapPriceTarget))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("baseTokenId", "targetTokenId", "maxOrderPerUser", "unitPriceBase", "unitPriceTarget") ++
                              Seq("signer")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte,
                                        DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(5.toByte),
    cdbvSet ++ Array(makerStateVar.index, 5.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(maxOrderPerUserStateVar.index, 2.toByte),
    cdbvSet ++ Array(unitPriceBaseStateVar.index, 3.toByte),
    cdbvSet ++ Array(unitPriceTargetStateVar.index, 4.toByte)
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId") ++
                                 Seq("baseTokenId", "targetTokenId", "isValidTokenId",
                                     "isBaseToken", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(7.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(11.toByte),
    conditionIf ++ Array(6.toByte, 7.toByte),
    conditionIf ++ Array(8.toByte, 10.toByte),
    assertTrue ++ Array(5.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // Withdraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId") ++
                                  Seq("baseTokenId", "targetTokenId", "isValidTokenId",
                                      "isBaseToken", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(7.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(11.toByte),
    conditionIf ++ Array(6.toByte, 7.toByte),
    conditionIf ++ Array(8.toByte, 10.toByte),
    assertTrue ++ Array(5.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Functions
  // Supersede
  val supersedeId: Short = 0
  val supersedePara: Seq[String] = Seq("newOwner",
                                       "maker")
  val supersedeDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val supersedeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeTextualBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Textual

}