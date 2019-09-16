package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import vsys.blockchain.state.opcdiffs.{AssertOpcDiff, LoadOpcDiff, OpcDiffer}
import vsys.utils.serialization.Deser

object ContractSystem {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(),
    Seq(sysSendFunc, sysDepositFunc, sysWithdrawFunc, sysTransferFunc),
    Seq(),
    Seq()
  ).right.get

  object FunId {
    val sysSend: Short = 0
    val sysDeposit: Short = 1
    val sysWithdraw: Short = 2
    val sysTransfer: Short = 3
  }

  object ProtoType {
    val sysSendParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val sysDepositParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)
    val sysWithdrawParaType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val sysTransferParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  }

  def listOpc(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Array[Byte] = {
    val length = Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort)
    val numOpc = Shorts.toByteArray(ids.length.toShort)
    val listOpc = ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten
    Bytes.concat(length, numOpc, listOpc)
  }

  object OpcId {
    val sysLoadSigner: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)
    val sysAssertIsSignerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsSignerOriginAssert.id.toByte)
    val sysTransfer: Array[Byte] = Array(OpcDiffer.OpcType.SystemOpc.id.toByte)
  }

  object DataStack {
    object sendInput {
      val recipientIndex: Byte = 0
      val amountIndex: Byte = 1
      val signerLoadIndex: Byte = 2
    }

    object transferInput {
      val senderIndex: Byte = 0
      val recipientIndex: Byte = 1
      val amountIndex: Byte = 2
    }

    object depositInput {
      val senderIndex: Byte = 0
      val smartIndex: Byte = 1
      val amountIndex: Byte = 2
    }

    object withdrawInput {
      val smartIndex: Byte = 0
      val recipientIndex: Byte = 1
      val amountIndex: Byte = 2
    }
  }

  object ListOpc {
    val sysLoadSignerIndex: Array[Byte] = Array(DataStack.sendInput.signerLoadIndex)
    val sysAssertIsSignerOriginTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex)
    val sysAssertIsSignerOriginDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex)
    val sysAssertIsSignerOriginWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.recipientIndex)
    val sysTransferSendIndex: Array[Byte] = Array(DataStack.sendInput.signerLoadIndex, DataStack.sendInput.recipientIndex, DataStack.sendInput.amountIndex)
    val sysTransferTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex, DataStack.transferInput.recipientIndex, DataStack.transferInput.amountIndex)
    val sysTransferDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex, DataStack.depositInput.smartIndex, DataStack.depositInput.amountIndex)
    val sysTransferWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.smartIndex, DataStack.withdrawInput.recipientIndex, DataStack.withdrawInput.amountIndex)

    // send
    val sysSend: List[Array[Byte]] = List(OpcId.sysLoadSigner, OpcId.sysTransfer)
    val sysSendIndex: List[Array[Byte]] = List(sysLoadSignerIndex, sysTransferSendIndex)
    // transfer
    val sysTransfer: List[Array[Byte]] = List(OpcId.sysAssertIsSignerOrigin, OpcId.sysTransfer)
    val sysTransferIndex: List[Array[Byte]] = List(sysAssertIsSignerOriginTransferIndex, sysTransferTransferIndex)
    // deposit
    val sysDeposit: List[Array[Byte]] = List(OpcId.sysAssertIsSignerOrigin, OpcId.sysTransfer)
    val sysDepositIndex: List[Array[Byte]] = List(sysAssertIsSignerOriginDepositIndex, sysTransferDepositIndex)
    // withdraw
    val sysWithdraw: List[Array[Byte]] = List(OpcId.sysAssertIsSignerOrigin, OpcId.sysTransfer)
    val sysWithdrawIndex: List[Array[Byte]] = List(sysAssertIsSignerOriginWithdrawIndex, sysTransferWithdrawIndex)
  }

  object OpcLine {
    val sysSendLine: Array[Byte] = listOpc(ListOpc.sysSend, ListOpc.sysSendIndex)
    val sysTransferLine: Array[Byte] = listOpc(ListOpc.sysTransfer, ListOpc.sysTransferIndex)
    val sysDepositLine: Array[Byte] = listOpc(ListOpc.sysDeposit, ListOpc.sysDepositIndex)
    val sysWithdrawLine: Array[Byte] = listOpc(ListOpc.sysWithdraw, ListOpc.sysWithdrawIndex)
  }

  def protoType(listReturnType: Array[Byte], listParaTypes: Array[Byte]): Array[Byte] = {
    val retType = Deser.serializeArray(listReturnType)
    val paraType = Deser.serializeArray(listParaTypes)
    Bytes.concat(retType, paraType)
  }

  lazy val nonReturnType: Array[Byte] = Array[Byte]()
  lazy val publicFuncType: Byte = 0
  lazy val sysSendFunc: Array[Byte] = Shorts.toByteArray(FunId.sysSend) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sysSendParaType) ++ OpcLine.sysSendLine
  lazy val sysTransferFunc: Array[Byte] = Shorts.toByteArray(FunId.sysTransfer) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sysTransferParaType) ++ OpcLine.sysTransferLine
  lazy val sysDepositFunc: Array[Byte] = Shorts.toByteArray(FunId.sysDeposit) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sysDepositParaType) ++ OpcLine.sysDepositLine
  lazy val sysWithdrawFunc: Array[Byte] = Shorts.toByteArray(FunId.sysWithdraw) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sysWithdrawParaType) ++ OpcLine.sysWithdrawLine
}
