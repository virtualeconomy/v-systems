package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVStableSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), // Triggers
    Seq(supersedeFunc, setOrderFunc, updateFunc, orderDepositFunc, orderWithdrawFunc, closeFunc,
        swapBaseToTargetFunc, swapTargetToBaseFunc), // Functions
    stateVarSeq, // StateVars
    stateMapSeq,  // StateMaps
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)  // Textual
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
  val stateMapOrderOwner               = List("orderOwner", "orderId", "owner")
  val stateMapFeeBase                  = List("feeBase", "orderId", "fee")
  val stateMapFeeTarget                = List("feeTarget", "orderId", "fee")
  val stateMapMinBase                  = List("minBase", "orderId", "amount")
  val stateMapMaxBase                  = List("maxBase", "orderId", "amount")
  val stateMapMinTarget                = List("minTarget", "orderId", "amount")
  val stateMapMaxTarget                = List("maxTarget", "orderId", "amount")
  val stateMapPriceBase                = List("priceBase", "orderId", "price")
  val stateMapPriceTarget              = List("priceTarget", "orderId", "price")
  val stateMapBaseTokenLocked          = List("baseTokenLocked", "orderId", "amount")
  val stateMapTargetTokenLocked        = List("targetTokenLocked", "orderId", "amount")
  val stateMapOrderStatus              = List("orderStatus", "orderId", "status")
  val baseTokenBalanceMap: StateMap    = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val targetTokenBalanceMap: StateMap  = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val userOrdersMap: StateMap          = StateMap(2.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val orderOwnerMap: StateMap          = StateMap(3.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val feeBaseMap: StateMap             = StateMap(4.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val feeTargetMap: StateMap           = StateMap(5.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val minBaseMap: StateMap             = StateMap(6.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val maxBaseMap: StateMap             = StateMap(7.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val minTargetMap: StateMap           = StateMap(8.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val maxTargetMap: StateMap           = StateMap(9.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val priceBaseMap: StateMap           = StateMap(10.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val priceTargetMap: StateMap         = StateMap(11.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val baseTokenLockedMap: StateMap     = StateMap(12.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val targetTokenLockedMap: StateMap   = StateMap(13.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderStatusMap: StateMap         = StateMap(14.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)

  lazy val stateMapSeq = Seq(baseTokenBalanceMap.arr, targetTokenBalanceMap.arr, userOrdersMap.arr, orderOwnerMap.arr,
                             feeBaseMap.arr, feeTargetMap.arr, minBaseMap.arr, maxBaseMap.arr,
                             minTargetMap.arr, maxTargetMap.arr, priceBaseMap.arr, priceTargetMap.arr,
                             baseTokenLockedMap.arr, targetTokenLockedMap.arr, orderStatusMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(
    Seq(stateMapBaseTokenBalance, stateMapTargetTokenBalance, stateMapUserOrders, stateMapOrderOwner,
        stateMapFeeBase, stateMapFeeTarget, stateMapMinBase, stateMapMaxBase, stateMapMinTarget,
        stateMapMaxTarget, stateMapPriceBase, stateMapPriceTarget, stateMapBaseTokenLocked, stateMapTargetTokenLocked, stateMapOrderStatus))

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
                                     "isBaseToken", "valueFalse", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        assertEqual ++ Array(5.toByte, 7.toByte),
        cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(8.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 9.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        assertEqual ++ Array(5.toByte, 7.toByte),
        cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(10.toByte),
    conditionIf ++ Array(6.toByte, 8.toByte),
    conditionIf ++ Array(9.toByte, 10.toByte),
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
    ), DataType.OpcBlock).bytes ++ Array(9.toByte),
    conditionIf ++ Array(6.toByte, 7.toByte),
    conditionIf ++ Array(8.toByte, 9.toByte),
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
  val supersedeOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeTextualBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Set Order
  val setOrderId: Short = 1
  val setOrderPara: Seq[String] = Seq("feeBase", "feeTarget", "minBase", "maxBase", "minTarget", "maxTarget", "priceBase",
                                      "priceTarget", "baseDeposit", "targetDeposit") ++
                                  Seq("caller", "maxOrderPerUser", "userOrders", "isValidOrderNum", "orderId", "amountOne", "valueTrue")
  val setOrderDataType: Array[Byte] = Array.fill[Byte](10)(DataType.Amount.id.toByte)
  val setOrderOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(10.toByte),
    cdbvrGet ++ Array(maxOrderPerUserStateVar.index, 11.toByte),
    cdbvrMapGetOrDefault ++ Array(userOrdersMap.index, 10.toByte, 12.toByte),
    compareGreater ++ Array(11.toByte, 12.toByte, 13.toByte),
    assertTrue ++ Array(13.toByte),
    loadTransactionId ++ Array(14.toByte),
    cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 10.toByte, 8.toByte),
    cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 10.toByte, 9.toByte),
    cdbvMapSet ++ Array(orderOwnerMap.index, 14.toByte, 10.toByte),
    cdbvMapSet ++ Array(feeBaseMap.index, 14.toByte, 0.toByte),
    cdbvMapSet ++ Array(feeTargetMap.index, 14.toByte, 1.toByte),
    cdbvMapSet ++ Array(minBaseMap.index, 14.toByte, 2.toByte),
    cdbvMapSet ++ Array(maxBaseMap.index, 14.toByte, 3.toByte),
    cdbvMapSet ++ Array(minTargetMap.index, 14.toByte, 4.toByte),
    cdbvMapSet ++ Array(maxTargetMap.index, 14.toByte, 5.toByte),
    cdbvMapSet ++ Array(priceBaseMap.index, 14.toByte, 6.toByte),
    cdbvMapSet ++ Array(priceTargetMap.index, 14.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(baseTokenLockedMap.index, 14.toByte, 8.toByte),
    cdbvMapValAdd ++ Array(targetTokenLockedMap.index, 14.toByte, 9.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(1L), DataType.Amount).bytes ++ Array(15.toByte),
    cdbvMapValAdd ++ Array(userOrdersMap.index, 10.toByte, 15.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(16.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 14.toByte, 16.toByte)
  )
  lazy val setOrderFunc: Array[Byte] = getFunctionBytes(setOrderId, publicFuncType, nonReturnType, setOrderDataType, setOrderOpcs)
  val setOrderTextualBytes: Array[Byte] = textualFunc("setOrder", Seq(), setOrderPara)

  private def commonCheckOpcs(ownerIndex: Byte, statusIndex: Byte): Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(orderOwnerMap.index, 0.toByte, ownerIndex),
    assertCaller ++ Array(ownerIndex),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, statusIndex),
    assertTrue ++ Array(statusIndex)
  )

  // Update Order
  val updateId: Short = 2
  val updatePara: Seq[String] = Seq("orderId", "feeBase", "feeTarget", "minBase", "maxBase", "minTarget", "maxTarget",
                                    "priceBase", "priceTarget") ++
                                Seq("owner", "status")
  val updateDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte) ++ Array.fill[Byte](8)(DataType.Amount.id.toByte)
  val updateOpcs: Seq[Array[Byte]] = commonCheckOpcs(9.toByte, 10.toByte) ++ Seq(
    cdbvMapSet ++ Array(feeBaseMap.index, 0.toByte, 1.toByte),
    cdbvMapSet ++ Array(feeTargetMap.index, 0.toByte, 2.toByte),
    cdbvMapSet ++ Array(minBaseMap.index, 0.toByte, 3.toByte),
    cdbvMapSet ++ Array(maxBaseMap.index, 0.toByte, 4.toByte),
    cdbvMapSet ++ Array(minTargetMap.index, 0.toByte, 5.toByte),
    cdbvMapSet ++ Array(maxTargetMap.index, 0.toByte, 6.toByte),
    cdbvMapSet ++ Array(priceBaseMap.index, 0.toByte, 7.toByte),
    cdbvMapSet ++ Array(priceTargetMap.index, 0.toByte, 8.toByte)
  )
  lazy val updateFunc: Array[Byte] = getFunctionBytes(updateId, publicFuncType, nonReturnType, updateDataType, updateOpcs)
  val updateTextualBytes: Array[Byte] = textualFunc("update", Seq(), updatePara)

  // Order Deposit
  val orderDepositId: Short = 3
  val orderDepositPara: Seq[String] = Seq("orderId", "baseDeposit", "targetDeposit") ++
                                      Seq("owner", "status")
  val orderDepositDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val orderDepositOpcs: Seq[Array[Byte]] = commonCheckOpcs(3.toByte, 4.toByte) ++ Seq(
    cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 3.toByte, 1.toByte),
    cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 3.toByte, 2.toByte),
    cdbvMapValAdd ++ Array(baseTokenLockedMap.index, 0.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(targetTokenLockedMap.index, 0.toByte, 2.toByte)
  )
  lazy val orderDepositFunc: Array[Byte] = getFunctionBytes(orderDepositId, publicFuncType, nonReturnType, orderDepositDataType, orderDepositOpcs)
  val orderDepositTextualBytes: Array[Byte] = textualFunc("orderDeposit", Seq(), orderDepositPara)

  // Order Withdraw
  val orderWithdrawId: Short = 4
  val orderWithdrawPara: Seq[String] = Seq("orderId", "baseWithdraw", "targetWithdraw") ++
                                       Seq("owner", "status")
  val orderWithdrawDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val orderWithdrawOpcs: Seq[Array[Byte]] = commonCheckOpcs(3.toByte, 4.toByte) ++ Seq(
    cdbvMapValMinus ++ Array(baseTokenLockedMap.index, 0.toByte, 1.toByte),
    cdbvMapValMinus ++ Array(targetTokenLockedMap.index, 0.toByte, 2.toByte),
    cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 3.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 3.toByte, 2.toByte)
  )
  lazy val orderWithdrawFunc: Array[Byte] = getFunctionBytes(orderWithdrawId, publicFuncType, nonReturnType, orderWithdrawDataType, orderWithdrawOpcs)
  val orderWithdrawTextualBytes: Array[Byte] = textualFunc("orderWithdraw", Seq(), orderWithdrawPara)

  // Order Close
  val closeId: Short = 5
  val closePara: Seq[String] = Seq("orderId") ++
                               Seq("owner", "status", "baseWithdraw", "targetWithdraw", "amountOne", "valueFalse")
  val closeDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)
  val closeOpcs: Seq[Array[Byte]] = commonCheckOpcs(1.toByte, 2.toByte) ++ Seq(
    cdbvrMapGetOrDefault ++ Array(baseTokenLockedMap.index, 0.toByte, 3.toByte),
    cdbvrMapGetOrDefault ++ Array(targetTokenLockedMap.index, 0.toByte, 4.toByte),
    cdbvMapValMinus ++ Array(baseTokenLockedMap.index, 0.toByte, 3.toByte),
    cdbvMapValMinus ++ Array(targetTokenLockedMap.index, 0.toByte, 4.toByte),
    cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 1.toByte, 3.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 1.toByte, 4.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(1L), DataType.Amount).bytes ++ Array(5.toByte),
    cdbvMapValMinus ++ Array(userOrdersMap.index, 1.toByte, 5.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(6.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 6.toByte)
  )
  lazy val closeFunc: Array[Byte] = getFunctionBytes(closeId, publicFuncType, nonReturnType, closeDataType, closeOpcs)
  val closeTextualBytes: Array[Byte] = textualFunc("close", Seq(), closePara)

  // Swap Common
  val swapCommonPara: Seq[String] = Seq("orderId", "amount", "fee", "price", "deadline") ++
                                    Seq("caller", "status", "lastBlockTime", "isValidTime", "feeInOrder", "priceInOrder",
                                        "minAmount", "isGreaterThanMin", "maxAmount", "isLessThanMax",
                                        "bigIntType", "unitPrice", "unitBigInt", "amountBigInt", "priceBigInt",
                                        "mul", "amountWithoutFeeBigInt", "amountType", "amountWithoutFee", "swapAmount",
                                        "amountZero", "isValidSwapAmount")
  val swapCommonDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte) ++
                                        Array.fill[Byte](3)(DataType.Amount.id.toByte) ++
                                        Array(DataType.Timestamp.id.toByte)
  private def swapCommonOpcs(feeIndex: Byte, priceIndex: Byte, minValue: Byte, maxValue: Byte, unitPriceIndex: Byte): Seq[Array[Byte]] = {
    Seq(
      loadCaller ++ Array(5.toByte),
      cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 6.toByte),
      assertTrue ++ Array(6.toByte),
      loadTimestamp ++ Array(7.toByte),
      compareGreaterEqual ++ Array(4.toByte, 7.toByte, 8.toByte),
      assertTrue ++ Array(8.toByte),
      cdbvrMapGet ++ Array(feeIndex, 0.toByte, 9.toByte),
      assertEqual ++ Array(2.toByte, 9.toByte),
      cdbvrMapGet ++ Array(priceIndex, 0.toByte, 10.toByte),
      assertEqual ++ Array(3.toByte, 10.toByte),
      cdbvrMapGet ++ Array(minValue, 0.toByte, 11.toByte),
      compareGreaterEqual ++ Array(1.toByte, 11.toByte, 12.toByte),
      assertTrue ++ Array(12.toByte),
      cdbvrMapGet ++ Array(maxValue, 0.toByte, 13.toByte),
      compareLessEqual ++ Array(1.toByte, 13.toByte, 14.toByte),
      assertTrue ++ Array(14.toByte),
      basicConstantGet ++ DataEntry(Array(DataType.BigInteger.id.toByte), DataType.DataTypeObj).bytes ++ Array(15.toByte),
      cdbvrGet ++ Array(unitPriceIndex, 16.toByte),
      basicConvert ++ Array(16.toByte, 15.toByte, 17.toByte),
      basicConvert ++ Array(1.toByte, 15.toByte, 18.toByte),
      basicConvert ++ Array(3.toByte, 15.toByte, 19.toByte),
      basicMultiply ++ Array(18.toByte, 19.toByte, 20.toByte),
      basicDivide ++ Array(20.toByte, 17.toByte, 21.toByte),
      basicConstantGet ++ DataEntry(Array(DataType.Amount.id.toByte), DataType.DataTypeObj).bytes ++ Array(22.toByte),
      basicConvert ++ Array(21.toByte, 22.toByte, 23.toByte),
      basicMinus ++ Array(23.toByte, 2.toByte, 24.toByte),
      basicConstantGet ++ DataEntry(Longs.toByteArray(0L), DataType.Amount).bytes ++ Array(25.toByte),
      compareGreater ++ Array(24.toByte, 25.toByte, 26.toByte),
      assertTrue ++ Array(26.toByte))
  }

  // Swap
  val swapBaseToTargetId: Short = 6
  val swapBaseToTargetOpcs: Seq[Array[Byte]] = swapCommonOpcs(feeBaseMap.index, priceBaseMap.index, minBaseMap.index, maxBaseMap.index, unitPriceBaseStateVar.index) ++ Seq(
    cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 5.toByte, 1.toByte),
    cdbvMapValMinus ++ Array(targetTokenLockedMap.index, 0.toByte, 24.toByte),
    cdbvMapValAdd ++ Array(baseTokenLockedMap.index, 0.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 5.toByte, 24.toByte)
  )
  lazy val swapBaseToTargetFunc: Array[Byte] = getFunctionBytes(swapBaseToTargetId, publicFuncType, nonReturnType, swapCommonDataType, swapBaseToTargetOpcs)
  val swapBaseToTargetTextualBytes: Array[Byte] = textualFunc("swapBaseToTarget", Seq(), swapCommonPara)

  val swapTargetToBaseId: Short = 7
  val swapTargetToBaseOpcs: Seq[Array[Byte]] = swapCommonOpcs(feeTargetMap.index, priceTargetMap.index, minTargetMap.index, maxTargetMap.index, unitPriceTargetStateVar.index) ++ Seq(
    cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 5.toByte, 1.toByte),
    cdbvMapValMinus ++ Array(baseTokenLockedMap.index, 0.toByte, 24.toByte),
    cdbvMapValAdd ++ Array(targetTokenLockedMap.index, 0.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 5.toByte, 24.toByte)
  )
  lazy val swapTargetToBaseFunc: Array[Byte] = getFunctionBytes(swapTargetToBaseId, publicFuncType, nonReturnType, swapCommonDataType, swapTargetToBaseOpcs)
  val swapTargetToBaseTextualBytes: Array[Byte] = textualFunc("swapTargetToBase", Seq(), swapCommonPara)

  // Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeTextualBytes, setOrderTextualBytes, updateTextualBytes,
    orderDepositTextualBytes, orderWithdrawTextualBytes, closeTextualBytes, swapBaseToTargetTextualBytes, swapTargetToBaseTextualBytes))


}