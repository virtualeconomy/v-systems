package vsys.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import scorex.serialization.Deser
import vsys.state.opcdiffs.{AssertOpcDiff, CDBVOpcDiff, CDBVROpcDiff, LoadOpcDiff, OpcDiffer, ReturnOpcDiff, TDBAOpcDiff, TDBAROpcDiff, TDBOpcDiff, TDBROpcDiff}

object ContractPermitted {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1), Seq(initFunc),
    Seq(supersedeFunc, issueFunc, destroyFunc, splitFunc, sendFunc, transferFunc, depositFunc, withdrawFunc,
      totalSupplyFunc, maxSupplyFunc, balanceOfFunc, getIssuerFunc),
    Seq(Array(StateVar.issuer, DataType.Address.id.toByte), Array(StateVar.maker, DataType.Address.id.toByte)),
    Seq(initializerTexture, descriptorTexture, stateVarTexture)
  ).right.get

  lazy val contractWithoutSplit: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1), Seq(initFunc),
    Seq(supersedeFuncWithoutSplit, issueFuncWithoutSplit, destroyFuncWithoutSplit, sendFuncWithoutSplit,
      transferFuncWithoutSplit, depositFuncWithoutSplit, withdrawFuncWithoutSplit, totalSupplyFuncWithoutSplit,
      maxSupplyFuncWithoutSplit, balanceOfFuncWithoutSplit, getIssuerFuncWithoutSplit),
    Seq(Array(StateVar.issuer, DataType.Address.id.toByte), Array(StateVar.maker, DataType.Address.id.toByte)),
    Seq(initializerTexture, descriptorTextureWithoutSplit, stateVarTexture)
  ).right.get

  object FunId {
    val init: Short = 0
    val supersede: Short = 0
    val issue: Short = 1
    val destroy: Short = 2
    val split: Short = 3
    val send: Short = 4
    val transfer: Short = 5
    val deposit: Short = 6
    val withdraw: Short = 7
    val totalSupply: Short = 8
    val maxSupply: Short = 9
    val balanceOf: Short = 10
    val getIssuer: Short = 11
  }

  object FunIdWithoutSplit {
    val init: Short = 0
    val supersede: Short = 0
    val issue: Short = 1
    val destroy: Short = 2
    val send: Short = 3
    val transfer: Short = 4
    val deposit: Short = 5
    val withdraw: Short = 6
    val totalSupply: Short = 7
    val maxSupply: Short = 8
    val balanceOf: Short = 9
    val getIssuer: Short = 10
  }

  object ProtoType {
    val initParaType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)
    val supersedeParaType: Array[Byte] = Array(DataType.Account.id.toByte)
    val issueParaType: Array[Byte] = Array(DataType.Amount.id.toByte)
    val destroyParaType: Array[Byte] = Array(DataType.Amount.id.toByte)
    val splitParaType: Array[Byte] = Array(DataType.Amount.id.toByte)
    val sendParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val transferParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val depositParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)
    val withdrawParaType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val totalSupplyParaType: Array[Byte] = Array()
    val maxSupplyParaType: Array[Byte] = Array()
    val balanceOfParaType: Array[Byte] = Array(DataType.Account.id.toByte)
    val getIssuerParaType: Array[Byte] = Array()
  }

  def listOpc(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Array[Byte] = {
    val length = Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort)
    val numOpc = Shorts.toByteArray(ids.length.toShort)
    val listOpc = ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten
    Bytes.concat(length, numOpc, listOpc)
  }

  object OpcId {
    val opcAssertGteqZero: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.GteqZeroAssert.id.toByte)
    val opcAssertLteq: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.LteqAssert.id.toByte)
    val opcAssertLtInt64: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.LtInt64Assert.id.toByte)
    val opcAssertGtZero: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.GtZeroAssert.id.toByte)
    val opcAssertEq: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.EqAssert.id.toByte)
    val opcAssertIsCallerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsCallerOriginAssert.id.toByte)
    val opcAssertIsSignerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsSignerOriginAssert.id.toByte)

    val opcLoadSigner: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)
    val opcLoadCaller: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.CallerLoad.id.toByte)

    val opcCDBVSet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVOpc.id.toByte, CDBVOpcDiff.CDBVType.SetCDBV.id.toByte)

    val opcCDBVRGet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVROpc.id.toByte, CDBVROpcDiff.CDBVRType.GetCDBVR.id.toByte)

    val opcTDBNewToken: Array[Byte] = Array(OpcDiffer.OpcType.TDBOpc.id.toByte, TDBOpcDiff.TDBType.NewTokenTDB.id.toByte)
    val opcTDBSplit: Array[Byte] = Array(OpcDiffer.OpcType.TDBOpc.id.toByte, TDBOpcDiff.TDBType.SplitTDB.id.toByte)

    val opcTDBROpcMax: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.MaxTDBR.id.toByte)
    val opcTDBROpcTotal: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.TotalTDBR.id.toByte)

    val opcTDBADeposit: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.DepositTDBA.id.toByte)
    val opcTDBAWithdraw: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.WithdrawTDBA.id.toByte)
    val opcTDBATransfer: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.TransferTDBA.id.toByte)

    val opcTDBARBalance: Array[Byte] = Array(OpcDiffer.OpcType.TDBAROpc.id.toByte, TDBAROpcDiff.TDBARType.BalanceTBDAR.id.toByte)

    val opcReturnValue: Array[Byte] = Array(OpcDiffer.OpcType.ReturnOpc.id.toByte, ReturnOpcDiff.ReturnType.ValueReturn.id.toByte)
  }

  object StateVar {
    val issuer: Byte = 0
    val maker: Byte = 1
  }

  object DataStack {
    object initInput {
      val maxIndex: Byte = 0
      val unityIndex: Byte = 1
      val shortTextIndex: Byte = 2
      val issuerLoadIndex: Byte = 3
    }

    object supersedeIndex {
      val newIssuerIndex: Byte = 0
      val maker: Byte = 1
    }

    object issueInput {
      val amountIndex: Byte = 0
      val issuerGetIndex: Byte = 1
    }

    object destroyInput {
      val amountIndex: Byte = 0
      val issuerGetIndex: Byte = 1
    }

    object splitInput {
      val amountIndex: Byte = 0
      val issuerGetIndex: Byte = 1
    }

    object sendInput {
      val receiptIndex: Byte = 0
      val amountIndex: Byte = 1
      val callerLoadIndex: Byte = 2
    }

    object transferInput {
      val senderIndex: Byte = 0
      val receiptIndex: Byte = 1
      val amountIndex: Byte = 2
    }

    object depositInput {
      val senderIndex: Byte = 0
      val smartIndex: Byte = 1
      val amountIndex: Byte = 2
    }

    object withdrawInput {
      val smartIndex: Byte = 0
      val receiptIndex: Byte = 1
      val amountIndex: Byte = 2
    }

    object balanceOfInput {
      val addressIndex: Byte = 0
    }

  }

  object ListOpc {
    val opcLoadSignerIndex: Array[Byte] = Array(3.toByte)
    val opcLoadCallerIndex: Array[Byte] = Array(2.toByte)

    val opcCDBVSetIssuerInitIndex: Array[Byte] = Array(StateVar.issuer, DataStack.initInput.issuerLoadIndex)
    val opcCDBVSetIssuerSupersedeIndex: Array[Byte] = Array(StateVar.issuer, DataStack.supersedeIndex.newIssuerIndex)
    val opcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)

    val opcCDBVRGetIssuerIndex: Array[Byte] = Array(StateVar.issuer, 1.toByte)
    val opcCDBVRGetMakerIndex: Array[Byte] = Array(StateVar.maker, 1.toByte)

    val opcAssertIsCallerOriginIssueIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex)
    val opcAssertIsCallerOriginDestroyIndex: Array[Byte] = Array(DataStack.destroyInput.issuerGetIndex)
    val opcAssertIsCallerOriginSplitIndex: Array[Byte] = Array(DataStack.splitInput.issuerGetIndex)
    val opcAssertIsCallerOriginTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex)
    val opcAssertIsCallerOriginDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex)
    val opcAssertIsCallerOriginWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.receiptIndex)
    val opcAssertIsMakerOriginSupersedeIndex: Array[Byte] = Array(DataStack.supersedeIndex.maker)

    val opcTDBNewTokenIndex: Array[Byte] = Array(DataStack.initInput.maxIndex, DataStack.initInput.unityIndex, DataStack.initInput.shortTextIndex)
    val opcTDBSplitIndex: Array[Byte] = Array(DataStack.splitInput.amountIndex)

    val opcTDBRTotalIndex: Array[Byte] = Array(0.toByte)
    val opcTDBRMaxIndex: Array[Byte] = Array(0.toByte)

    val opcTDBADepositIssueIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex, DataStack.issueInput.amountIndex)
    val opcTDBAWithdrawDestroyIndex: Array[Byte] = Array(DataStack.destroyInput.issuerGetIndex, DataStack.destroyInput.amountIndex)
    val opcTDBATransferSendIndex: Array[Byte] = Array(DataStack.sendInput.callerLoadIndex, DataStack.sendInput.receiptIndex, DataStack.sendInput.amountIndex)
    val opcTDBATransferTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex, DataStack.transferInput.receiptIndex, DataStack.transferInput.amountIndex)
    val opcTDBATransferDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex, DataStack.depositInput.smartIndex, DataStack.depositInput.amountIndex)
    val opcTDBATransferWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.smartIndex, DataStack.withdrawInput.receiptIndex, DataStack.withdrawInput.amountIndex)

    val opcTDBARBalanceOfIndex: Array[Byte] = Array(DataStack.balanceOfInput.addressIndex, 1.toByte)

    // init
    val initOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet, OpcId.opcTDBNewToken)
    val initOpcIndex: List[Array[Byte]] = List(opcLoadSignerIndex, opcCDBVSetIssuerInitIndex, opcCDBVSetMakerIndex, opcTDBNewTokenIndex)
    // supersede index and opc
    val supersedeOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsSignerOrigin, OpcId.opcCDBVSet)
    val supersedeOpcIndex: List[Array[Byte]] = List(opcCDBVRGetMakerIndex, opcAssertIsMakerOriginSupersedeIndex, opcCDBVSetIssuerSupersedeIndex)

    // issue
    val issueOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBADeposit)
    val issueOpcIndex: List[Array[Byte]] = List(opcCDBVRGetIssuerIndex, opcAssertIsCallerOriginIssueIndex, opcTDBADepositIssueIndex)
    // destroy
    val destroyOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBAWithdraw)
    val destroyOpcIndex: List[Array[Byte]] = List(opcCDBVRGetIssuerIndex, opcAssertIsCallerOriginDestroyIndex, opcTDBAWithdrawDestroyIndex)

    // split
    val splitOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBSplit)
    val splitOpcIndex: List[Array[Byte]] = List(opcCDBVRGetIssuerIndex, opcAssertIsCallerOriginSplitIndex, opcTDBSplitIndex)

    // send
    val sendOpc: List[Array[Byte]] = List(OpcId.opcLoadCaller, OpcId.opcTDBATransfer)
    val sendOpcIndex: List[Array[Byte]] = List(opcLoadCallerIndex, opcTDBATransferSendIndex)
    // transfer
    val transferOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
    val transferOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginTransferIndex, opcTDBATransferTransferIndex)
    // deposit
    val depositOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
    val depositOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginDepositIndex, opcTDBATransferDepositIndex)
    // withdraw
    val withdrawOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
    val withdrawOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginWithdrawIndex, opcTDBATransferWithdrawIndex)
    // totalSupply
    val totalSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcTotal, OpcId.opcReturnValue)
    val totalSupplyOpcIndex: List[Array[Byte]] = List(opcTDBRTotalIndex, Array(0.toByte))
    // maxSupplyOpc
    val maxSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcMax, OpcId.opcReturnValue)
    val maxSupplyOpcIndex: List[Array[Byte]] = List(opcTDBRMaxIndex, Array(0.toByte))
    // balanceOfOpc
    val balanceOfOpc: List[Array[Byte]] = List(OpcId.opcTDBARBalance, OpcId.opcReturnValue)
    val balanceOfOpcIndex: List[Array[Byte]] = List(opcTDBARBalanceOfIndex, Array(1.toByte))
    // getIssuerOpc
    val getIssuerOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcReturnValue)
    val getIssuerOpcIndex: List[Array[Byte]] = List(Array(StateVar.issuer, 0.toByte), Array(0.toByte))
  }

  object OpcLine {
    val initOpcLine: Array[Byte] = listOpc(ListOpc.initOpc, ListOpc.initOpcIndex)
    val supersedeOpcLine: Array[Byte] = listOpc(ListOpc.supersedeOpc, ListOpc.supersedeOpcIndex)
    val issueOpcLine: Array[Byte] = listOpc(ListOpc.issueOpc, ListOpc.issueOpcIndex)
    val destroyOpcLine: Array[Byte] = listOpc(ListOpc.destroyOpc, ListOpc.destroyOpcIndex)
    val splitOpcLine: Array[Byte] = listOpc(ListOpc.splitOpc, ListOpc.splitOpcIndex)
    val sendOpcLine: Array[Byte] = listOpc(ListOpc.sendOpc, ListOpc.sendOpcIndex)
    val transferOpcLine: Array[Byte] = listOpc(ListOpc.transferOpc, ListOpc.transferOpcIndex)
    val depositOpcLine: Array[Byte] = listOpc(ListOpc.depositOpc, ListOpc.depositOpcIndex)
    val withdrawOpcLine: Array[Byte] = listOpc(ListOpc.withdrawOpc, ListOpc.withdrawOpcIndex)
    val totalSupplyOpcLine: Array[Byte] = listOpc(ListOpc.totalSupplyOpc, ListOpc.totalSupplyOpcIndex)
    val maxSupplyOpcLine: Array[Byte] = listOpc(ListOpc.maxSupplyOpc, ListOpc.maxSupplyOpcIndex)
    val balanceOfOpcLine: Array[Byte] = listOpc(ListOpc.balanceOfOpc, ListOpc.balanceOfOpcIndex)
    val getIssuerOpcLine: Array[Byte] = listOpc(ListOpc.getIssuerOpc, ListOpc.getIssuerOpcIndex)
  }

  def protoType(listReturnType: Array[Byte], listParaTypes: Array[Byte]): Array[Byte] = {
    val retType = Deser.serializeArray(listReturnType)
    val paraType = Deser.serializeArray(listParaTypes)
    Bytes.concat(retType, paraType)
  }

  lazy val nonReturnType: Array[Byte] = Array[Byte]()
  lazy val onInitTriggerType: Byte = 0
  lazy val publicFuncType: Byte = 0
  lazy val initFunc: Array[Byte] = Shorts.toByteArray(FunId.init) ++ Array(onInitTriggerType) ++ protoType(nonReturnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val supersedeFunc: Array[Byte] = Shorts.toByteArray(FunId.supersede) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.supersedeParaType) ++ OpcLine.supersedeOpcLine
  lazy val issueFunc: Array[Byte] = Shorts.toByteArray(FunId.issue) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.issueParaType) ++ OpcLine.issueOpcLine
  lazy val destroyFunc: Array[Byte] = Shorts.toByteArray(FunId.destroy) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.destroyParaType) ++ OpcLine.destroyOpcLine
  lazy val splitFunc: Array[Byte] = Shorts.toByteArray(FunId.split) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.splitParaType) ++ OpcLine.splitOpcLine
  lazy val sendFunc: Array[Byte] = Shorts.toByteArray(FunId.send) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sendParaType) ++ OpcLine.sendOpcLine
  lazy val transferFunc: Array[Byte] = Shorts.toByteArray(FunId.transfer) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.transferParaType) ++ OpcLine.transferOpcLine
  lazy val depositFunc: Array[Byte] = Shorts.toByteArray(FunId.deposit) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawFunc: Array[Byte] = Shorts.toByteArray(FunId.withdraw) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine
  lazy val totalSupplyFunc: Array[Byte] = Shorts.toByteArray(FunId.totalSupply) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.totalSupplyParaType) ++ OpcLine.totalSupplyOpcLine
  lazy val maxSupplyFunc: Array[Byte] = Shorts.toByteArray(FunId.maxSupply) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.maxSupplyParaType) ++ OpcLine.maxSupplyOpcLine
  lazy val balanceOfFunc: Array[Byte] = Shorts.toByteArray(FunId.balanceOf) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.balanceOfParaType) ++ OpcLine.balanceOfOpcLine
  lazy val getIssuerFunc: Array[Byte] = Shorts.toByteArray(FunId.getIssuer) ++ Array(publicFuncType) ++ protoType(Array(DataType.Account.id.toByte), ProtoType.getIssuerParaType) ++ OpcLine.getIssuerOpcLine

  lazy val initFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.init) ++ Array(onInitTriggerType) ++ protoType(nonReturnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val supersedeFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.supersede) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.supersedeParaType) ++ OpcLine.supersedeOpcLine
  lazy val issueFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.issue) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.issueParaType) ++ OpcLine.issueOpcLine
  lazy val destroyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.destroy) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.destroyParaType) ++ OpcLine.destroyOpcLine
  lazy val sendFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.send) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.sendParaType) ++ OpcLine.sendOpcLine
  lazy val transferFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.transfer) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.transferParaType) ++ OpcLine.transferOpcLine
  lazy val depositFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.deposit) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.withdraw) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine
  lazy val totalSupplyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.totalSupply) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.totalSupplyParaType) ++ OpcLine.totalSupplyOpcLine
  lazy val maxSupplyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.maxSupply) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.maxSupplyParaType) ++ OpcLine.maxSupplyOpcLine
  lazy val balanceOfFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.balanceOf) ++ Array(publicFuncType) ++ protoType(Array(DataType.Amount.id.toByte), ProtoType.balanceOfParaType) ++ OpcLine.balanceOfOpcLine
  lazy val getIssuerFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.getIssuer) ++ Array(publicFuncType) ++ protoType(Array(DataType.Account.id.toByte), ProtoType.getIssuerParaType) ++ OpcLine.getIssuerOpcLine

  def textureFunc(name: String, ret: Seq[String], para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x))))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  object ParaName {
    val initPara: Seq[String] = Seq("max", "unity", "tokenDescription", "signer")
    val supersedePara: Seq[String] = Seq("newIssuer", "maker")
    val issuePara: Seq[String] = Seq("amount", "issuer")
    val destroyPara: Seq[String] = Seq("amount", "issuer")
    val splitPara: Seq[String] = Seq("newUnity", "issuer")
    val sendPara: Seq[String] = Seq("receipt", "amount", "caller")
    val transferPara: Seq[String]= Seq("sender", "receipt", "amount")
    val depositPara: Seq[String] = Seq("sender", "smart", "amount")
    val withdrawPara: Seq[String]= Seq("smart", "receipt", "amount")
    val totalSupplyPara: Seq[String] = Seq("total")
    val maxSupplyPara: Seq[String] = Seq("max")
    val balanceOfPara: Seq[String] = Seq("address", "balance")
    val getIssuerPara: Seq[String] = Seq("issuer")
  }

  val stateVarName = List("issuer", "maker")
  lazy val stateVarTexture: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  val initFuncBytes: Array[Byte] = textureFunc("init", Seq(), ParaName.initPara)
  val supersedeFuncBytes: Array[Byte] = textureFunc("supersede", Seq(), ParaName.supersedePara)
  val issueFuncBytes: Array[Byte] = textureFunc("issue", Seq(), ParaName.issuePara)
  val destroyFuncBytes: Array[Byte] = textureFunc("destroy", Seq(), ParaName.destroyPara)
  val splitFuncBytes: Array[Byte] = textureFunc("split", Seq(), ParaName.splitPara)
  val sendFuncBytes: Array[Byte] = textureFunc("send", Seq(), ParaName.sendPara)
  val transferFuncBytes: Array[Byte] = textureFunc("transfer", Seq(), ParaName.transferPara)
  val depositFuncBytes: Array[Byte] = textureFunc("deposit", Seq(), ParaName.depositPara)
  val withdrawFuncBytes: Array[Byte] = textureFunc("withdraw", Seq(), ParaName.withdrawPara)
  val totalSupplyFuncBytes: Array[Byte] = textureFunc("totalSupply", Seq("total"), ParaName.totalSupplyPara)
  val maxSupplyFuncBytes: Array[Byte] = textureFunc("maxSupply", Seq("max"), ParaName.maxSupplyPara)
  val balanceOfFuncBytes: Array[Byte] = textureFunc("balanceOf", Seq("balance"), ParaName.balanceOfPara)
  val getIssuerFuncBytes: Array[Byte] = textureFunc("getIssuer", Seq("issuer"), ParaName.getIssuerPara)

  lazy val initializerTexture: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes))
  lazy val descriptorTexture: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, splitFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))
  lazy val descriptorTextureWithoutSplit: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))

}
