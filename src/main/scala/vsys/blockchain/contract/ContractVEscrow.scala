package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVEscrow {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), // Triggers
    Seq(supersedeFunc, createFunc, recipientDepositFunc, judgeDepositFunc, payerCancelFunc, recipientCancelFunc, judgeCancelFunc,
      submitWorkFunc, approveWorkFunc, applyToJudgeFunc, judgeFunc, submitPenaltyFunc,
      payerRefundFunc, recipientRefundFunc, collectFunc),
    stateVarSeq, // StateVars
    stateMapSeq, // StateMaps
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
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
  val stateMapContractBalance                 = List("contractBalance", "userAddress", "balance")
  val stateMapOrderPayer                      = List("orderPayer", "orderId", "payer")
  val stateMapOrderRecipient                  = List("orderRecipient", "orderId", "recipient")
  val stateMapOrderAmount                     = List("orderAmount", "orderId", "amount")
  val stateMapOrderRecipientDeposit           = List("orderRecipientDeposit", "orderId", "repDeposit")
  val stateMapOrderJudgeDeposit               = List("orderJudgeDeposit", "orderId", "judgeDeposit")
  val stateMapOrderFee                        = List("orderFee", "orderId", "fee")
  val stateMapOrderRecipientAmount            = List("orderRecipientAmount", "orderId", "repAmount")
  val stateMapOrderRefund                     = List("orderRefund", "orderId", "refund")
  val stateMapOrderRecipientRefund            = List("orderRecipientRefund", "orderId", "repRefund")
  val stateMapOrderExpirationTime             = List("orderExpirationTime", "orderId", "expirationTime")
  val stateMapOrderStatus                     = List("orderStatus", "orderId", "status")
  val stateMapOrderRepDepositStatus           = List("orderRepDepositStatus", "orderId", "repDepositStatus")
  val stateMapOrderJudgeDepositStatus         = List("orderJudgeDepositStatus", "orderId", "judgeDepositStatus")
  val stateMapOrderSubmitStatus               = List("orderSubmitStatus", "orderId", "submitStatus")
  val stateMapOrderJudgeStatus                = List("orderJudgeStatus", "orderId", "judgeStatus")
  val stateMapOrderRepLockedAmount            = List("orderRepLockedAmount", "orderId", "repLockedAmount")
  val stateMapOrderJudgeLockedAmount          = List("orderJudgeLockedAmount", "orderId", "judgeLockedAmount")
  val contractBalanceMap: StateMap            = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val orderPayerMap: StateMap                 = StateMap(1.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val orderRecipientMap: StateMap             = StateMap(2.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val orderAmountMap: StateMap                = StateMap(3.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderRecipientDepositMap: StateMap      = StateMap(4.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderJudgeDepositMap: StateMap          = StateMap(5.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderFeeMap: StateMap                   = StateMap(6.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderRecipientAmountMap: StateMap       = StateMap(7.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderRefundMap: StateMap                = StateMap(8.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderRecipientRefundMap: StateMap       = StateMap(9.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderExpirationTimeMap: StateMap        = StateMap(10.toByte, DataType.ShortBytes.id.toByte, DataType.Timestamp.id.toByte)
  val orderStatusMap: StateMap                = StateMap(11.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  val orderRepDepositStatusMap: StateMap      = StateMap(12.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  val orderJudgeDepositStatusMap: StateMap    = StateMap(13.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  val orderSubmitStatusMap: StateMap          = StateMap(14.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  val orderJudgeStatusMap: StateMap           = StateMap(15.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
  val orderRepLockedAmountMap: StateMap       = StateMap(16.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val orderJudgeLockedAmountMap: StateMap     = StateMap(17.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)

  lazy val stateMapSeq = Seq(contractBalanceMap.arr, orderPayerMap.arr, orderRecipientMap.arr, orderAmountMap.arr,
                             orderRecipientDepositMap.arr, orderJudgeDepositMap.arr, orderFeeMap.arr,
                             orderRecipientAmountMap.arr, orderRefundMap.arr, orderRecipientRefundMap.arr,
                             orderExpirationTimeMap.arr, orderStatusMap.arr, orderRepDepositStatusMap.arr, orderJudgeDepositStatusMap.arr,
                             orderSubmitStatusMap.arr, orderJudgeStatusMap.arr, orderRepLockedAmountMap.arr, orderJudgeLockedAmountMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(
    Seq(stateMapContractBalance, stateMapOrderPayer, stateMapOrderRecipient, stateMapOrderAmount,
        stateMapOrderRecipientDeposit, stateMapOrderJudgeDeposit, stateMapOrderFee, stateMapOrderRecipientAmount,
        stateMapOrderRefund, stateMapOrderRecipientRefund, stateMapOrderExpirationTime, stateMapOrderStatus,
        stateMapOrderRepDepositStatus, stateMapOrderJudgeDepositStatus, stateMapOrderSubmitStatus, stateMapOrderJudgeStatus,
        stateMapOrderRepLockedAmount, stateMapOrderJudgeLockedAmount))

  val commonDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("tokenId", "duration", "judgeDuration",
                                  "signer")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.Timestamp.id.toByte, DataType.Timestamp.id.toByte)
  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(3.toByte),
    cdbvSet ++ Array(makerStateVar.index, 3.toByte),
    cdbvSet ++ Array(judgeStateVar.index, 3.toByte),
    cdbvSet ++ Array(tokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(durationStateVar.index, 1.toByte),
    cdbvSet ++ Array(judgeDurationStateVar.index, 2.toByte)
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId",
                                     "contractTokenId")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(tokenIdStateVar.index, 3.toByte),
    assertEqual ++ Array(2.toByte, 3.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 0.toByte, 1.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // Withdraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId",
                                      "contractTokenId")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(tokenIdStateVar.index, 3.toByte),
    assertEqual ++ Array(2.toByte, 3.toByte),
    cdbvMapValMinus ++ Array(contractBalanceMap.index, 0.toByte, 1.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Functions
  // Supersede
  val supersedeId: Short = 0
  val supersedePara: Seq[String] = Seq("newJudge",
                                       "maker")
  val supersedeDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val supersedeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(judgeStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeTextualBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Create Order Function
  val createId: Short = 1
  val createPara: Seq[String] = Seq("recipient", "amount", "repDeposit", "judgeDeposit", "fee", "refund", "expirationTime") ++
                                Seq("caller", "orderId", "repAmount", "needToDeposit", "totalDeposit", "repRefund",
                                    "valueTrue", "valueFalse", "amountZero")
  val createDataType: Array[Byte] = Array(DataType.Address.id.toByte) ++
                                    Array.fill[Byte](5)(DataType.Amount.id.toByte) ++
                                    Array(DataType.Timestamp.id.toByte)
  val createOpcs: Seq[Array[Byte]] =  Seq(
    loadCaller ++ Array(7.toByte),
    loadTransactionId ++ Array(8.toByte),
    cdbvMapValMinus ++ Array(contractBalanceMap.index, 7.toByte, 1.toByte),
    cdbvMapSet ++ Array(orderPayerMap.index, 8.toByte, 7.toByte),
    cdbvMapSet ++ Array(orderRecipientMap.index, 8.toByte, 0.toByte),
    cdbvMapSet ++ Array(orderAmountMap.index, 8.toByte, 1.toByte),
    cdbvMapSet ++ Array(orderRecipientDepositMap.index, 8.toByte, 2.toByte),
    cdbvMapSet ++ Array(orderJudgeDepositMap.index, 8.toByte, 3.toByte),
    cdbvMapSet ++ Array(orderFeeMap.index, 8.toByte, 4.toByte),
    basicMinus ++ Array(1.toByte, 4.toByte, 9.toByte),
    cdbvMapSet ++ Array(orderRecipientAmountMap.index, 8.toByte, 9.toByte),
    basicAdd ++ Array(2.toByte, 3.toByte, 10.toByte),
    basicAdd ++ Array(10.toByte, 1.toByte, 11.toByte),
    basicMinus ++ Array(11.toByte, 5.toByte, 12.toByte),
    cdbvMapSet ++ Array(orderRefundMap.index, 8.toByte, 5.toByte),
    cdbvMapSet ++ Array(orderRecipientRefundMap.index, 8.toByte, 12.toByte),
    cdbvMapSet ++ Array(orderExpirationTimeMap.index, 8.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(13.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 8.toByte, 13.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(14.toByte),
    cdbvMapSet ++ Array(orderRepDepositStatusMap.index, 8.toByte, 14.toByte),
    cdbvMapSet ++ Array(orderJudgeDepositStatusMap.index, 8.toByte, 14.toByte),
    cdbvMapSet ++ Array(orderSubmitStatusMap.index, 8.toByte, 14.toByte),
    cdbvMapSet ++ Array(orderJudgeStatusMap.index, 8.toByte, 14.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(0L), DataType.Amount).bytes ++ Array(15.toByte),
    cdbvMapSet ++ Array(orderRepLockedAmountMap.index, 8.toByte, 15.toByte),
    cdbvMapSet ++ Array(orderJudgeLockedAmountMap.index, 8.toByte, 15.toByte)
  )
  lazy val createFunc: Array[Byte] = getFunctionBytes(createId, publicFuncType, nonReturnType, createDataType, createOpcs)
  val createTextualBytes: Array[Byte] = textualFunc("create", Seq(), createPara)

  // Order Deposit Common
  private def depositCommonOpcs(isCallerJudge: Boolean, orderDepositStatusIndex: Byte, orderDepositAmountIndex: Byte, orderLockedAmountIndex: Byte): Seq[Array[Byte]] = {
    val tmp :Seq[Array[Byte]] = Seq(
      assertCaller ++ Array(1.toByte),
      cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
      assertTrue ++ Array(2.toByte),
      cdbvrMapGet ++ Array(orderDepositStatusIndex, 0.toByte, 3.toByte),
      basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(4.toByte),
      assertEqual ++ Array(3.toByte, 4.toByte),
      cdbvrMapGet ++ Array(orderDepositAmountIndex, 0.toByte, 5.toByte),
      cdbvMapValMinus ++ Array(contractBalanceMap.index, 1.toByte, 5.toByte),
      basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(6.toByte),
      cdbvMapSet ++ Array(orderDepositStatusIndex, 0.toByte, 6.toByte),
      cdbvMapSet ++ Array(orderLockedAmountIndex, 0.toByte, 5.toByte)
    )
    if (isCallerJudge) {
      Seq(cdbvrGet ++ Array(judgeStateVar.index, 1.toByte)) ++ tmp
    } else {
      Seq(cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 1.toByte)) ++ tmp
    }
  }

  // Recipient Deposit Function
  val recipientDepositId: Short = 2
  val recipientDepositPara: Seq[String] = Seq("orderId") ++
                                          Seq("recipient", "orderStatus", "depositStatus", "valueFalse", "amount", "valueTrue")
  val recipientDepositOpcs: Seq[Array[Byte]] =  depositCommonOpcs(false, orderRepDepositStatusMap.index,
                                                                  orderRecipientDepositMap.index, orderRepLockedAmountMap.index)
  lazy val recipientDepositFunc: Array[Byte] = getFunctionBytes(recipientDepositId, publicFuncType, nonReturnType, commonDataType, recipientDepositOpcs)
  val recipientDepositTextualBytes: Array[Byte] = textualFunc("recipientDeposit", Seq(), recipientDepositPara)

  // Judge Deposit Function
  val judgeDepositId: Short = 3
  val judgeDepositPara: Seq[String] = Seq("orderId") ++
                                      Seq("judge", "orderStatus", "depositStatus", "valueFalse", "amount", "valueTrue")
  val judgeDepositOpcs: Seq[Array[Byte]] =  depositCommonOpcs(true, orderJudgeDepositStatusMap.index,
                                                              orderJudgeDepositMap.index, orderJudgeLockedAmountMap.index)
  lazy val judgeDepositFunc: Array[Byte] = getFunctionBytes(judgeDepositId, publicFuncType, nonReturnType, commonDataType, judgeDepositOpcs)
  val judgeDepositTextualBytes: Array[Byte] = textualFunc("judgeDeposit", Seq(), judgeDepositPara)

  // Order Cancel Common
  val cancelCommonPara: Seq[String] = Seq("orderId") ++
                                      Seq("payer", "recipient", "judge", "orderStatus", "repDepositStatus", "judgeDepositStatus",
                                          "depositStatus", "valueFalse", "amount", "recipientAmount", "judgeAmount")
  private def cancelCommonOpcs(callerIndex: Byte): Seq[Array[Byte]] = {
    Seq(
      cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 1.toByte),
      cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 2.toByte),
      cdbvrGet ++ Array(judgeStateVar.index, 3.toByte),
      assertCaller ++ Array(callerIndex),
      cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 4.toByte),
      assertTrue ++ Array(4.toByte),
      cdbvrMapGet ++ Array(orderRepDepositStatusMap.index, 0.toByte, 5.toByte),
      cdbvrMapGet ++ Array(orderJudgeDepositStatusMap.index, 0.toByte, 6.toByte),
      basicAnd ++ Array(5.toByte, 6.toByte, 7.toByte),
      basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(8.toByte),
      assertEqual ++ Array(7.toByte, 8.toByte),
      cdbvrMapGet ++ Array(orderAmountMap.index, 0.toByte, 9.toByte),
      cdbvMapValAdd ++ Array(contractBalanceMap.index, 1.toByte, 9.toByte),
      cdbvrMapGet ++ Array(orderRepLockedAmountMap.index, 0.toByte, 10.toByte),
      cdbvMapValAdd ++ Array(contractBalanceMap.index, 2.toByte, 10.toByte),
      cdbvrMapGet ++ Array(orderJudgeLockedAmountMap.index, 0.toByte, 11.toByte),
      cdbvMapValAdd ++ Array(contractBalanceMap.index, 3.toByte, 11.toByte),
      cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 8.toByte)
    )
  }

  // Payer Cancel Function
  val payerCancelId: Short = 4
  val payerCancelOpcs: Seq[Array[Byte]] = cancelCommonOpcs(1.toByte)
  lazy val payerCancelFunc: Array[Byte] = getFunctionBytes(payerCancelId, publicFuncType, nonReturnType, commonDataType, payerCancelOpcs)
  val payerCancelTextualBytes: Array[Byte] = textualFunc("payerCancel", Seq(), cancelCommonPara)

  // Recipient Cancel Function
  val recipientCancelId: Short = 5
  val recipientCancelOpcs: Seq[Array[Byte]] = cancelCommonOpcs(2.toByte)
  lazy val recipientCancelFunc: Array[Byte] = getFunctionBytes(recipientCancelId, publicFuncType, nonReturnType, commonDataType, recipientCancelOpcs)
  val recipientCancelTextualBytes: Array[Byte] = textualFunc("recipientCancel", Seq(), cancelCommonPara)

  // Judge Cancel Function
  val judgeCancelId: Short = 6
  val judgeCancelOpcs: Seq[Array[Byte]] = cancelCommonOpcs(3.toByte)
  lazy val judgeCancelFunc: Array[Byte] = getFunctionBytes(judgeCancelId, publicFuncType, nonReturnType, commonDataType, judgeCancelOpcs)
  val judgeCancelTextualBytes: Array[Byte] = textualFunc("judgeCancel", Seq(), cancelCommonPara)

  private def timestampCheck(startIndex: Int, isValid: Boolean): Seq[Array[Byte]] = {
    val a: Byte = if (isValid) (startIndex + 1).toByte else startIndex.toByte
    val b: Byte = if (isValid) startIndex.toByte else (startIndex + 1).toByte
    Seq(
      loadTimestamp ++ Array(startIndex.toByte),
      cdbvrMapGet ++ Array(orderExpirationTimeMap.index, 0.toByte, (startIndex + 1).toByte),
      compareGreater ++ Array(a, b, (startIndex + 2).toByte),
      assertTrue ++Array((startIndex + 2).toByte),
    )
  }

  // Submit Work Function
  val submitWorkId: Short = 7
  val submitWorkPara: Seq[String] = Seq("orderId") ++
                                    Seq("recipient", "orderStatus", "repDepositStatus", "judgeDepositStatus", "depositStatus",
                                        "currentTime", "expirationTime", "isValidTime", "valueFalse", "submitStatus", "duration",
                                        "time", "updateTime", "valueTrue")
  val submitWorkOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    cdbvrMapGet ++ Array(orderRepDepositStatusMap.index, 0.toByte, 3.toByte),
    cdbvrMapGet ++ Array(orderJudgeDepositStatusMap.index, 0.toByte, 4.toByte),
    basicAnd ++ Array(3.toByte, 4.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte)) ++ timestampCheck(6, true) ++ Seq(
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(9.toByte),
    cdbvrMapGet ++ Array(orderSubmitStatusMap.index, 0.toByte, 10.toByte),
    assertEqual ++ Array(9.toByte, 10.toByte),
    cdbvrGet ++ Array(durationStateVar.index, 11.toByte),
    basicAdd ++ Array(6.toByte, 11.toByte, 12.toByte),
    basicMax ++ Array(12.toByte, 7.toByte, 13.toByte),
    cdbvMapSet ++ Array(orderExpirationTimeMap.index, 0.toByte, 13.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(14.toByte),
    cdbvMapSet ++ Array(orderSubmitStatusMap.index, 0.toByte, 14.toByte)
  )
  lazy val submitWorkFunc: Array[Byte] = getFunctionBytes(submitWorkId, publicFuncType, nonReturnType, commonDataType, submitWorkOpcs)
  val submitWorkTextualBytes: Array[Byte] = textualFunc("submitWork", Seq(), submitWorkPara)

  // Approve Work Function
  val approveWorkId: Short = 8
  val approveWorkPara: Seq[String] = Seq("orderId") ++
                                     Seq("payer", "orderStatus", "isSubmit", "currentTime", "expirationTime", "isValidTime",
                                         "recipient", "judge", "workAmount", "recipientLocked", "recipientAmount",
                                         "fee", "judgeLocked", "judgeAmount", "valueFalse")
  val approveWorkOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    cdbvrMapGet ++ Array(orderSubmitStatusMap.index, 0.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte)) ++ timestampCheck(4, true) ++ Seq(
    cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 7.toByte),
    cdbvrGet ++ Array(judgeStateVar.index, 8.toByte),
    cdbvrMapGet ++ Array(orderRecipientAmountMap.index, 0.toByte, 9.toByte),
    cdbvrMapGet ++ Array(orderRepLockedAmountMap.index, 0.toByte, 10.toByte),
    basicAdd ++ Array(9.toByte, 10.toByte, 11.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 7.toByte, 11.toByte),
    cdbvrMapGet ++ Array(orderFeeMap.index, 0.toByte, 12.toByte),
    cdbvrMapGet ++ Array(orderJudgeLockedAmountMap.index, 0.toByte, 13.toByte),
    basicAdd ++ Array(12.toByte, 13.toByte, 14.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 8.toByte, 14.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(15.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 15.toByte)
  )
  lazy val approveWorkFunc: Array[Byte] = getFunctionBytes(approveWorkId, publicFuncType, nonReturnType, commonDataType, approveWorkOpcs)
  val approveWorkTextualBytes: Array[Byte] = textualFunc("approveWork", Seq(), approveWorkPara)

  // Apply to Judge Function
  val applyToJudgeId: Short = 9
  val applyToJudgePara: Seq[String] = Seq("orderId") ++
                                      Seq("payer", "orderStatus", "isSubmit", "currentTime", "expirationTime", "isValidTime",
                                          "judgeStatus", "valueFalse", "judgeDuration", "time", "updateTime", "valueTrue")
  val applyToJudgeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    cdbvrMapGet ++ Array(orderSubmitStatusMap.index, 0.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte)) ++ timestampCheck(4, true) ++ Seq(
    cdbvrMapGet ++ Array(orderJudgeStatusMap.index, 0.toByte, 7.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(8.toByte),
    assertEqual ++ Array(7.toByte, 8.toByte),
    cdbvrGet ++ Array(judgeDurationStateVar.index, 9.toByte),
    basicAdd ++ Array(4.toByte, 9.toByte, 10.toByte),
    basicMax ++ Array(10.toByte, 5.toByte, 11.toByte),
    cdbvMapSet ++ Array(orderExpirationTimeMap.index, 0.toByte, 11.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(12.toByte),
    cdbvMapSet ++ Array(orderJudgeStatusMap.index, 0.toByte, 12.toByte)
  )
  lazy val applyToJudgeFunc: Array[Byte] = getFunctionBytes(applyToJudgeId, publicFuncType, nonReturnType, commonDataType, applyToJudgeOpcs)
  val applyToJudgeTextualBytes: Array[Byte] = textualFunc("applyToJudge", Seq(), applyToJudgePara)

  // Judge Function
  val judgeId: Short = 10
  val judgePara: Seq[String] = Seq("orderId", "payerAmount", "recipientAmount") ++
                               Seq("judge", "orderStatus", "currentTime", "expirationTime", "isValidTime",
                                   "judgeStatus", "payToRep", "recipientLocked", "totalToPay", "totalArrange",
                                   "payer", "recipient", "fee", "judgeLocked", "judgeAmount", "valueFalse")
  val judgeDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val judgeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(judgeStateVar.index, 3.toByte),
    assertCaller ++ Array(3.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 4.toByte),
    assertTrue ++ Array(4.toByte)) ++ timestampCheck(5, true) ++ Seq(
    cdbvrMapGet ++ Array(orderJudgeStatusMap.index, 0.toByte, 8.toByte),
    assertTrue ++ Array(8.toByte),
    cdbvrMapGet ++ Array(orderRecipientAmountMap.index, 0.toByte, 9.toByte),
    cdbvrMapGet ++ Array(orderRepLockedAmountMap.index, 0.toByte, 10.toByte),
    basicAdd ++ Array(9.toByte, 10.toByte, 11.toByte),
    basicAdd ++ Array(1.toByte, 2.toByte, 12.toByte),
    assertEqual ++ Array(11.toByte, 12.toByte),
    cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 13.toByte),
    cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 14.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 13.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 14.toByte, 2.toByte),
    cdbvrMapGet ++ Array(orderFeeMap.index, 0.toByte, 15.toByte),
    cdbvrMapGet ++ Array(orderJudgeLockedAmountMap.index, 0.toByte, 16.toByte),
    basicAdd ++ Array(15.toByte, 16.toByte, 17.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 3.toByte, 17.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(18.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 18.toByte)
  )
  lazy val judgeFunc: Array[Byte] = getFunctionBytes(judgeId, publicFuncType, nonReturnType, judgeDataType, judgeOpcs)
  val judgeTextualBytes: Array[Byte] = textualFunc("judge", Seq(), judgePara)

  // Submit Penalty Function
  val submitPenaltyId: Short = 11
  val submitPenaltyPara: Seq[String] = Seq("orderId") ++
                                       Seq("payer", "orderStatus", "valueFalse", "isSubmit",
                                           "currentTime", "expirationTime", "isExpired",
                                           "judge", "workAmount", "recipientLocked", "penaltyAmount",
                                           "fee", "judgeLocked", "judgeAmount")
  val submitPenaltyOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(3.toByte),
    cdbvrMapGet ++ Array(orderSubmitStatusMap.index, 0.toByte, 4.toByte),
    assertEqual ++ Array(3.toByte, 4.toByte)) ++ timestampCheck(5, false) ++ Seq(
    cdbvrGet ++ Array(judgeStateVar.index, 8.toByte),
    cdbvrMapGet ++ Array(orderRecipientAmountMap.index, 0.toByte, 9.toByte),
    cdbvrMapGet ++ Array(orderRepLockedAmountMap.index, 0.toByte, 10.toByte),
    basicAdd ++ Array(9.toByte, 10.toByte, 11.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 1.toByte, 11.toByte),
    cdbvrMapGet ++ Array(orderFeeMap.index, 0.toByte, 12.toByte),
    cdbvrMapGet ++ Array(orderJudgeLockedAmountMap.index, 0.toByte, 13.toByte),
    basicAdd ++ Array(12.toByte, 13.toByte, 14.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 8.toByte, 14.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 3.toByte)
  )
  lazy val submitPenaltyFunc: Array[Byte] = getFunctionBytes(submitPenaltyId, publicFuncType, nonReturnType, commonDataType, submitPenaltyOpcs)
  val submitPenaltyTextualBytes: Array[Byte] = textualFunc("submitPenalty", Seq(), submitPenaltyPara)

  // Order Refund Common
  val refundCommonPara: Seq[String] = Seq("orderId") ++
                                      Seq("payer", "recipient", "orderStatus", "judgeStatus", "currentTime", "expirationTime", "isExpired",
                                          "recipientRefund", "payerRefund", "valueFalse")
  private def refundCommonOpcs(callerIndex: Byte): Seq[Array[Byte]] = {
    Seq(
      cdbvrMapGet ++ Array(orderPayerMap.index, 0.toByte, 1.toByte),
      cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 2.toByte),
      assertCaller ++ Array(callerIndex),
      cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 3.toByte),
      assertTrue ++ Array(3.toByte),
      cdbvrMapGet ++ Array(orderJudgeStatusMap.index, 0.toByte, 4.toByte),
      assertTrue ++ Array(4.toByte)) ++ timestampCheck(5, false) ++ Seq(
      cdbvrMapGet ++ Array(orderRecipientRefundMap.index, 0.toByte, 8.toByte),
      cdbvMapValAdd ++ Array(contractBalanceMap.index, 2.toByte, 8.toByte),
      cdbvrMapGet ++ Array(orderRefundMap.index, 0.toByte, 9.toByte),
      cdbvMapValAdd ++ Array(contractBalanceMap.index, 1.toByte, 9.toByte),
      basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(10.toByte),
      cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 10.toByte)
    )
  }

  // Payer Refund Function
  val payerRefundId: Short = 12
  val payerRefundOpcs: Seq[Array[Byte]] = refundCommonOpcs(1.toByte)
  lazy val payerRefundFunc: Array[Byte] = getFunctionBytes(payerRefundId, publicFuncType, nonReturnType, commonDataType, payerRefundOpcs)
  val payerRefundTextualBytes: Array[Byte] = textualFunc("payerRefund", Seq(), refundCommonPara)

  // Payer Refund Function
  val recipientRefundId: Short = 13
  val recipientRefundOpcs: Seq[Array[Byte]] = refundCommonOpcs(2.toByte)
  lazy val recipientRefundFunc: Array[Byte] = getFunctionBytes(recipientRefundId, publicFuncType, nonReturnType, commonDataType, recipientRefundOpcs)
  val recipientRefundTextualBytes: Array[Byte] = textualFunc("recipientRefund", Seq(), refundCommonPara)

  // Collect Function
  val collectId: Short = 14
  val collectPara: Seq[String] = Seq("orderId") ++
                                 Seq("recipient", "orderStatus", "isSubmit", "valueFalse", "judgeStatus",
                                     "currentTime", "expirationTime", "isExpired",
                                     "judge", "workAmount", "recipientLocked", "recipientAmount", "fee", "judgeLocked", "judgeAmount")
  val collectOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(orderRecipientMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrMapGet ++ Array(orderStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    cdbvrMapGet ++ Array(orderSubmitStatusMap.index, 0.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(4.toByte),
    cdbvrMapGet ++ Array(orderJudgeStatusMap.index, 0.toByte, 5.toByte),
    assertEqual ++ Array(4.toByte, 5.toByte)) ++ timestampCheck(6, false) ++ Seq(
    cdbvrGet ++ Array(judgeStateVar.index, 9.toByte),
    cdbvrMapGet ++ Array(orderRecipientAmountMap.index, 0.toByte, 10.toByte),
    cdbvrMapGet ++ Array(orderRepLockedAmountMap.index, 0.toByte, 11.toByte),
    basicAdd ++ Array(10.toByte, 11.toByte, 12.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 1.toByte, 12.toByte),
    cdbvrMapGet ++ Array(orderFeeMap.index, 0.toByte, 13.toByte),
    cdbvrMapGet ++ Array(orderJudgeLockedAmountMap.index, 0.toByte, 14.toByte),
    basicAdd ++ Array(13.toByte, 14.toByte, 15.toByte),
    cdbvMapValAdd ++ Array(contractBalanceMap.index, 9.toByte, 15.toByte),
    cdbvMapSet ++ Array(orderStatusMap.index, 0.toByte, 4.toByte)
  )
  lazy val collectFunc: Array[Byte] = getFunctionBytes(collectId, publicFuncType, nonReturnType, commonDataType, collectOpcs)
  val collectTextualBytes: Array[Byte] = textualFunc("collect", Seq(), collectPara)

  // Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeTextualBytes, createTextualBytes, recipientDepositTextualBytes,
    judgeDepositTextualBytes, payerCancelTextualBytes, recipientCancelTextualBytes, judgeCancelTextualBytes, submitWorkTextualBytes,
    approveWorkTextualBytes, applyToJudgeTextualBytes, judgeTextualBytes, submitPenaltyTextualBytes,
    payerRefundTextualBytes, recipientRefundTextualBytes, collectTextualBytes))
}