package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen.{StateMap, StateVar, textualStateMap}
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
  
  // Initialization Trigger

  // Deposit Trigger

  // WithDraw Trigger

  // Functions

  // Textual
}