package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractSystem {
  lazy val contract: Contract = getContract(Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(),
    Seq(sysSendFunc, sysDepositFunc, sysWithdrawFunc, sysTransferFunc),
    Seq(), Seq(),
    Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ))

  //sysSend
  val sysSendId: Short = 0
  val sysSendPara: Seq[String] = Seq("recipient", "amount",
                                     "caller")
  val sysSendDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val sysSendOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(2.toByte),
    sysTransfer ++ Array(2.toByte, 0.toByte, 1.toByte))
  lazy val sysSendFunc: Array[Byte] = getFunctionBytes(sysSendId, publicFuncType, nonReturnType, sysSendDataType, sysSendOpcs)
  val sysSendFuncBytes: Array[Byte] = textualFunc("send", Seq(), sysSendPara)

  //sysDeposit
  val sysDepositId: Short = 1
  val sysDepositPara: Seq[String] = Seq("sender", "smart", "amount")
  val sysDepositDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)
  val sysDepositOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    sysTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val sysDepositFunc: Array[Byte] = getFunctionBytes(sysDepositId, publicFuncType, nonReturnType, sysDepositDataType, sysDepositOpcs)
  val sysDepositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), sysDepositPara)

  //sysWithdraw
  val sysWithdrawId: Short = 2
  val sysWithdrawPara: Seq[String] = Seq("smart", "recipient", "amount")
  val sysWithdrawDataType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val sysWithdrawOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(1.toByte),
    sysTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val sysWithdrawFunc: Array[Byte] = getFunctionBytes(sysWithdrawId, publicFuncType, nonReturnType, sysWithdrawDataType, sysWithdrawOpcs)
  val sysWithdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), sysWithdrawPara)

  //sysTransfer
  val sysTransferId: Short = 3
  val sysTransferPara: Seq[String] = Seq("sender", "recipient", "amount")
  val sysTransferDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val sysTransferOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    sysTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val sysTransferFunc: Array[Byte] = getFunctionBytes(sysTransferId, publicFuncType, nonReturnType, sysTransferDataType, sysTransferOpcs)
  val sysTransferFuncBytes: Array[Byte] = textualFunc("transfer", Seq(), sysTransferPara)

  //textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq())
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(sysSendFuncBytes, sysDepositFuncBytes, sysWithdrawFuncBytes, sysTransferFuncBytes))
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(Seq())
}
