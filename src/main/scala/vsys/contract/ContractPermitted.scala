package vsys.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import scorex.serialization.Deser
import vsys.state.opcdiffs.{AssertOpcDiff, CDBVOpcDiff, CDBVROpcDiff, LoadOpcDiff, OpcDiffer, TDBAOpcDiff, TDBAROpcDiff, TDBOpcDiff, TDBROpcDiff}

object ContractPermitted {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1), initFunc,
    Seq(supersedeFunc, issueFunc, destroyFunc, splitFunc, sendFunc, transferFunc, depositFunc, withdrawFunc,
      totalSupplyFunc, maxSupplyFunc, balanceOfFunc, getIssuerFunc),
    Seq(Array(StateVar.issuer, DataType.Address.id.toByte), Array(StateVar.maker, DataType.Address.id.toByte),
      Array(StateVar.max, DataType.Amount.id.toByte), Array(StateVar.total, DataType.Amount.id.toByte),
      Array(StateVar.unity, DataType.Amount.id.toByte), Array(StateVar.shortText, DataType.ShortText.id.toByte)),
    Seq(initializerTexture, descriptorTexture, stateVarTexture)
  ).right.get

  lazy val contractWithoutSplit: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1), initFunc,
    Seq(supersedeFuncWithoutSplit, issueFuncWithoutSplit, destroyFuncWithoutSplit, sendFuncWithoutSplit,
      transferFuncWithoutSplit, depositFuncWithoutSplit, withdrawFuncWithoutSplit, totalSupplyFuncWithoutSplit,
      maxSupplyFuncWithoutSplit, balanceOfFuncWithoutSplit, getIssuerFuncWithoutSplit),
    Seq(Array(StateVar.issuer, DataType.Address.id.toByte), Array(StateVar.maker, DataType.Address.id.toByte),
      Array(StateVar.max, DataType.Amount.id.toByte), Array(StateVar.total, DataType.Amount.id.toByte),
      Array(StateVar.unity, DataType.Amount.id.toByte), Array(StateVar.shortText, DataType.ShortText.id.toByte)),
    Seq(initializerTexture, descriptorTextureWithoutSplit, stateVarTexture)
  ).right.get

  object FunId {
    val init: Short = 0
    val supersede: Short = 1
    val issue: Short = 2
    val destroy: Short = 3
    val split: Short = 4
    val send: Short = 5
    val transfer: Short = 6
    val deposit: Short = 7
    val withdraw: Short = 8
    val totalSupply: Short = 9
    val maxSupply: Short = 10
    val balanceOf: Short = 11
    val getIssuer: Short = 12
  }

  object FunIdWithoutSplit {
    val init: Short = 0
    val supersede: Short = 1
    val issue: Short = 2
    val destroy: Short = 3
    val send: Short = 4
    val transfer: Short = 5
    val deposit: Short = 6
    val withdraw: Short = 7
    val totalSupply: Short = 8
    val maxSupply: Short = 9
    val balanceOf: Short = 10
    val getIssuer: Short = 11
  }

  object ProtoType {
    val initParaType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)
    val supersedeParaType: Array[Byte] = Array(DataType.Account.id.toByte)
    val issueParaType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val destroyParaType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val splitParaType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val sendParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val transferParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val depositParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val withdrawParaType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
    val totalSupplyParaType: Array[Byte] = Array(DataType.Int32.id.toByte)
    val maxSupplyParaType: Array[Byte] = Array(DataType.Int32.id.toByte)
    val balanceOfParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Int32.id.toByte)
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

    val opcTDBROpcGet: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.GetTDBR.id.toByte)
    val opcTDBROpcTotal: Array[Byte] = Array(OpcDiffer.OpcType.TDBROpc.id.toByte, TDBROpcDiff.TDBRType.TotalTDBR.id.toByte)

    val opcTDBADeposit: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.DepositTDBA.id.toByte)
    val opcTDBAWithdraw: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.WithdrawTDBA.id.toByte)
    val opcTDBATransfer: Array[Byte] = Array(OpcDiffer.OpcType.TDBAOpc.id.toByte, TDBAOpcDiff.TDBAType.TransferTDBA.id.toByte)

    val opcTDBARBalance: Array[Byte] = Array(OpcDiffer.OpcType.TDBAROpc.id.toByte, TDBAROpcDiff.TDBARType.BalanceTBDAR.id.toByte)

    val opcReturnValue: Array[Byte] = Array(OpcDiffer.OpcType.ReturnOpc.id.toByte, 0x00)
  }

  object StateVar {
    val issuer: Byte = 0
    val maker: Byte = 1
    val max: Byte = 2
    val total: Byte = 3
    val unity: Byte = 4
    val shortText: Byte = 5
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
      val tokenIndex: Byte = 1
      val issuerGetIndex: Byte = 2
    }

    object destroyInput {
      val amountIndex: Byte = 0
      val tokenIndex: Byte = 1
      val issuerGetIndex: Byte = 2
    }

    object splitInput {
      val amountIndex: Byte = 0
      val tokenIndex: Byte = 1
      val issuerGetIndex: Byte = 2
    }

    object sendInput {
      val receiptIndex: Byte = 0
      val amountIndex: Byte = 1
      val tokenIndex: Byte = 2
      val callerLoadIndex: Byte = 3
    }

    object transferInput {
      val senderIndex: Byte = 0
      val receiptIndex: Byte = 1
      val amountIndex: Byte = 2
      val tokenIndex: Byte = 3
    }

    object depositInput {
      val senderIndex: Byte = 0
      val smartIndex: Byte = 1
      val amountIndex: Byte = 2
      val tokenIndex: Byte = 3
    }

    object withdrawInput {
      val smartIndex: Byte = 0
      val receiptIndex: Byte = 1
      val amountIndex: Byte = 2
      val tokenIndex: Byte = 3
    }

    object totalSupplyInput {
      val tokenIndex: Byte = 0
    }

    object maxSupplyInput {
      val tokenIndex: Byte = 0
    }

    object balanceOfInput {
      val addressIndex: Byte = 0
      val tokenIndex: Byte = 1
    }

  }

  object ListOpc {
    val opcLoadSignerIndex: Array[Byte] = Array()
    val opcLoadCallerIndex: Array[Byte] = Array()

    val opcCDBVSetIssuerInitIndex: Array[Byte] = Array(StateVar.issuer, DataStack.initInput.issuerLoadIndex)
    val opcCDBVSetIssuerSupersedeIndex: Array[Byte] = Array(StateVar.issuer, DataStack.supersedeIndex.newIssuerIndex)
    val opcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)

    val opcCDBVRGetIssuerIndex: Array[Byte] = Array(StateVar.issuer)
    val opcCDBVRGetMakerIndex: Array[Byte] = Array(StateVar.maker)

    val opcAssertIsCallerOriginIssueIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex)
    val opcAssertIsCallerOriginDestroyIndex: Array[Byte] = Array(DataStack.destroyInput.issuerGetIndex)
    val opcAssertIsCallerOriginSplitIndex: Array[Byte] = Array(DataStack.splitInput.issuerGetIndex)
    val opcAssertIsCallerOriginTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex)
    val opcAssertIsCallerOriginDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex)
    val opcAssertIsCallerOriginWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.receiptIndex)
    val opcAssertIsMakerOriginSupersedeIndex: Array[Byte] = Array(DataStack.supersedeIndex.maker)

    val opcTDBNewTokenIndex: Array[Byte] = Array(StateVar.max, StateVar.total, StateVar.unity, StateVar.shortText,
      DataStack.initInput.maxIndex, DataStack.initInput.unityIndex, DataStack.initInput.shortTextIndex)
    val opcTDBSplitIndex: Array[Byte] = Array(StateVar.unity, DataStack.splitInput.amountIndex, DataStack.splitInput.tokenIndex)

    val opcTDBRTotalIndex: Array[Byte] = Array(StateVar.total, DataStack.totalSupplyInput.tokenIndex)
    val opcTDBRMaxIndex: Array[Byte] = Array(StateVar.max, DataStack.maxSupplyInput.tokenIndex)

    val opcTDBADepositIssueIndex: Array[Byte] = Array(StateVar.max, StateVar.total, DataStack.issueInput.issuerGetIndex,
      DataStack.issueInput.amountIndex, DataStack.issueInput.tokenIndex)
    val opcTDBAWithdrawDestroyIndex: Array[Byte] = Array(StateVar.total, DataStack.destroyInput.issuerGetIndex,
      DataStack.destroyInput.amountIndex, DataStack.destroyInput.tokenIndex)
    val opcTDBATransferSendIndex: Array[Byte] = Array(DataStack.sendInput.callerLoadIndex, DataStack.sendInput.receiptIndex, DataStack.sendInput.amountIndex, DataStack.sendInput.tokenIndex)
    val opcTDBATransferTransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex, DataStack.transferInput.receiptIndex, DataStack.transferInput.amountIndex, DataStack.transferInput.tokenIndex)
    val opcTDBATransferDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex, DataStack.depositInput.smartIndex, DataStack.depositInput.amountIndex, DataStack.depositInput.tokenIndex)
    val opcTDBATransferWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.smartIndex, DataStack.withdrawInput.receiptIndex, DataStack.withdrawInput.amountIndex, DataStack.withdrawInput.tokenIndex)

    val opcTDBARBalanceOfIndex: Array[Byte] = Array(DataStack.balanceOfInput.addressIndex, DataStack.balanceOfInput.tokenIndex)

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
    val totalSupplyOpcIndex: List[Array[Byte]] = List(opcTDBRTotalIndex, Array())
    // maxSupplyOpc
    val maxSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcGet, OpcId.opcReturnValue)
    val maxSupplyOpcIndex: List[Array[Byte]] = List(opcTDBRMaxIndex, Array())
    // balanceOfOpc
    val balanceOfOpc: List[Array[Byte]] = List(OpcId.opcTDBARBalance, OpcId.opcReturnValue)
    val balanceOfOpcIndex: List[Array[Byte]] = List(opcTDBARBalanceOfIndex, Array())
    // getIssuerOpc
    val getIssuerOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcReturnValue)
    val getIssuerOpcIndex: List[Array[Byte]] = List(opcCDBVRGetIssuerIndex, Array())
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

  def protoType(returnType: Byte, listParaTypes: Array[Byte]): Array[Byte] = {
    val len = Shorts.toByteArray((listParaTypes.length + 1).toShort)
    val retType = Array(returnType)
    val listPT = listParaTypes
    Bytes.concat(len, retType, listPT)
  }

  lazy val returnType: Byte = 1
  lazy val initFunc: Array[Byte] = Shorts.toByteArray(FunId.init) ++ protoType(returnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val supersedeFunc: Array[Byte] = Shorts.toByteArray(FunId.supersede) ++ protoType(returnType, ProtoType.supersedeParaType) ++ OpcLine.supersedeOpcLine
  lazy val issueFunc: Array[Byte] = Shorts.toByteArray(FunId.issue) ++ protoType(returnType, ProtoType.issueParaType) ++ OpcLine.issueOpcLine
  lazy val destroyFunc: Array[Byte] = Shorts.toByteArray(FunId.destroy) ++ protoType(returnType, ProtoType.destroyParaType) ++ OpcLine.destroyOpcLine
  lazy val splitFunc: Array[Byte] = Shorts.toByteArray(FunId.split) ++ protoType(returnType, ProtoType.splitParaType) ++ OpcLine.splitOpcLine
  lazy val sendFunc: Array[Byte] = Shorts.toByteArray(FunId.send) ++ protoType(returnType, ProtoType.sendParaType) ++ OpcLine.sendOpcLine
  lazy val transferFunc: Array[Byte] = Shorts.toByteArray(FunId.transfer) ++ protoType(returnType, ProtoType.transferParaType) ++ OpcLine.transferOpcLine
  lazy val depositFunc: Array[Byte] = Shorts.toByteArray(FunId.deposit) ++ protoType(returnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawFunc: Array[Byte] = Shorts.toByteArray(FunId.withdraw) ++ protoType(returnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine
  lazy val totalSupplyFunc: Array[Byte] = Shorts.toByteArray(FunId.totalSupply) ++ protoType(returnType, ProtoType.totalSupplyParaType) ++ OpcLine.totalSupplyOpcLine
  lazy val maxSupplyFunc: Array[Byte] = Shorts.toByteArray(FunId.maxSupply) ++ protoType(returnType, ProtoType.maxSupplyParaType) ++ OpcLine.maxSupplyOpcLine
  lazy val balanceOfFunc: Array[Byte] = Shorts.toByteArray(FunId.balanceOf) ++ protoType(returnType, ProtoType.balanceOfParaType) ++ OpcLine.balanceOfOpcLine
  lazy val getIssuerFunc: Array[Byte] = Shorts.toByteArray(FunId.getIssuer) ++ protoType(returnType, ProtoType.getIssuerParaType) ++ OpcLine.getIssuerOpcLine

  lazy val initFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.init) ++ protoType(returnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val supersedeFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.supersede) ++ protoType(returnType, ProtoType.supersedeParaType) ++ OpcLine.supersedeOpcLine
  lazy val issueFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.issue) ++ protoType(returnType, ProtoType.issueParaType) ++ OpcLine.issueOpcLine
  lazy val destroyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.destroy) ++ protoType(returnType, ProtoType.destroyParaType) ++ OpcLine.destroyOpcLine
  lazy val sendFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.send) ++ protoType(returnType, ProtoType.sendParaType) ++ OpcLine.sendOpcLine
  lazy val transferFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.transfer) ++ protoType(returnType, ProtoType.transferParaType) ++ OpcLine.transferOpcLine
  lazy val depositFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.deposit) ++ protoType(returnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.withdraw) ++ protoType(returnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine
  lazy val totalSupplyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.totalSupply) ++ protoType(returnType, ProtoType.totalSupplyParaType) ++ OpcLine.totalSupplyOpcLine
  lazy val maxSupplyFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.maxSupply) ++ protoType(returnType, ProtoType.maxSupplyParaType) ++ OpcLine.maxSupplyOpcLine
  lazy val balanceOfFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.balanceOf) ++ protoType(returnType, ProtoType.balanceOfParaType) ++ OpcLine.balanceOfOpcLine
  lazy val getIssuerFuncWithoutSplit: Array[Byte] = Shorts.toByteArray(FunIdWithoutSplit.getIssuer) ++ protoType(returnType, ProtoType.getIssuerParaType) ++ OpcLine.getIssuerOpcLine

  def textureFunc(name: String, ret: String, para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serilizeString(ret))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  object ParaName {
    val initPara: Seq[String] = Seq("max", "unity", "tokenDescription")
    val supersedePara: Seq[String] = Seq("newIssuer")
    val issuePara: Seq[String] = Seq("amount", "tokenIndex")
    val destroyPara: Seq[String] = Seq("amount", "tokenIndex")
    val splitPara: Seq[String] = Seq("newUnity", "tokenIndex")
    val sendPara: Seq[String] = Seq("receipt", "amount", "tokenIndex")
    val transferPara: Seq[String]= Seq("sender", "receipt", "amount", "tokenIndex")
    val depositPara: Seq[String] = Seq("sender", "smart", "amount", "tokenIndex")
    val withdrawPara: Seq[String]= Seq("smart", "receipt", "amount", "tokenIndex")
    val totalSupplyPara: Seq[String] = Seq("tokenIndex")
    val maxSupplyPara: Seq[String] = Seq("tokenIndex")
    val balanceOfPara: Seq[String] = Seq("address", "tokenIndex")
    val getIssuerPara: Seq[String] = Seq()
  }

  val stateVarName = List("issuer", "maker", "max", "total", "unity", "description")
  lazy val stateVarTexture: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  val initFuncBytes: Array[Byte] = textureFunc("init", "void", ParaName.initPara)
  val supersedeFuncBytes: Array[Byte] = textureFunc("supersede", "void", ParaName.supersedePara)
  val issueFuncBytes: Array[Byte] = textureFunc("issue", "void", ParaName.issuePara)
  val destroyFuncBytes: Array[Byte] = textureFunc("destroy", "void", ParaName.destroyPara)
  val splitFuncBytes: Array[Byte] = textureFunc("split", "void", ParaName.splitPara)
  val sendFuncBytes: Array[Byte] = textureFunc("send", "void", ParaName.sendPara)
  val transferFuncBytes: Array[Byte] = textureFunc("transfer", "void", ParaName.transferPara)
  val depositFuncBytes: Array[Byte] = textureFunc("deposit", "void", ParaName.depositPara)
  val withdrawFuncBytes: Array[Byte] = textureFunc("withdraw", "void", ParaName.withdrawPara)
  val totalSupplyFuncBytes: Array[Byte] = textureFunc("totalSupply", "amount", ParaName.totalSupplyPara)
  val maxSupplyFuncBytes: Array[Byte] = textureFunc("maxSupply", "amount", ParaName.maxSupplyPara)
  val balanceOfFuncBytes: Array[Byte] = textureFunc("balanceOf", "amount", ParaName.balanceOfPara)
  val getIssuerFuncBytes: Array[Byte] = textureFunc("getIssuer", "issuer", ParaName.getIssuerPara)

  lazy val initializerTexture: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes))
  lazy val descriptorTexture: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, splitFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))
  lazy val descriptorTextureWithoutSplit: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))

}
