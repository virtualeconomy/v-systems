package vsys.blockchain.contract.vescrow

import com.google.common.primitives.Bytes
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.state.ByteStr

trait VEscrowFunctionHelperGen extends VEscrowContractGen {

  def getEscrowContractStateVarKeys(vEscrowContractId: Array[Byte]): Seq[ByteStr] = {
    val makerKey = ByteStr(Bytes.concat(vEscrowContractId, Array(0.toByte)))
    val judgeKey = ByteStr(Bytes.concat(vEscrowContractId, Array(1.toByte)))
    val tokenIdKey = ByteStr(Bytes.concat(vEscrowContractId, Array(2.toByte)))
    val durationKey = ByteStr(Bytes.concat(vEscrowContractId, Array(3.toByte)))
    val judgeDurationKey = ByteStr(Bytes.concat(vEscrowContractId, Array(4.toByte)))

    Seq(makerKey, judgeKey, tokenIdKey, durationKey, judgeDurationKey)
  }

  def getEscrowContractStateMapKeys(vEscrowContractId: Array[Byte], orderId: Array[Byte]): Seq[ByteStr] = {
    val orderPayerKey = ByteStr(Bytes.concat(vEscrowContractId, Array(1.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRecipientKey = ByteStr(Bytes.concat(vEscrowContractId, Array(2.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderAmountKey = ByteStr(Bytes.concat(vEscrowContractId, Array(3.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRecipientDepositKey = ByteStr(Bytes.concat(vEscrowContractId, Array(4.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderJudgeDepositKey = ByteStr(Bytes.concat(vEscrowContractId, Array(5.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderFeeKey = ByteStr(Bytes.concat(vEscrowContractId, Array(6.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRecipientAmountKey = ByteStr(Bytes.concat(vEscrowContractId, Array(7.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRefundKey = ByteStr(Bytes.concat(vEscrowContractId, Array(8.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRecipientRefundKey = ByteStr(Bytes.concat(vEscrowContractId, Array(9.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderExpirationTimeKey = ByteStr(Bytes.concat(vEscrowContractId, Array(10.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderStatusKey = ByteStr(Bytes.concat(vEscrowContractId, Array(11.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRepDepositStatusKey = ByteStr(Bytes.concat(vEscrowContractId, Array(12.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderJudgeDepositStatusKey = ByteStr(Bytes.concat(vEscrowContractId, Array(13.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderSubmitStatusKey = ByteStr(Bytes.concat(vEscrowContractId, Array(14.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderJudgeStatusKey = ByteStr(Bytes.concat(vEscrowContractId, Array(15.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderRepLockedAmountKey = ByteStr(Bytes.concat(vEscrowContractId, Array(16.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderJudgeLockedAmountKey = ByteStr(Bytes.concat(vEscrowContractId, Array(17.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))

    Seq(orderPayerKey, orderRecipientKey, orderAmountKey, orderRecipientDepositKey, orderJudgeDepositKey, orderFeeKey,
      orderRecipientAmountKey, orderRefundKey, orderRecipientRefundKey, orderExpirationTimeKey, orderStatusKey, orderRepDepositStatusKey,
      orderJudgeDepositStatusKey, orderSubmitStatusKey, orderJudgeStatusKey, orderRepLockedAmountKey, orderJudgeLockedAmountKey)
  }
}