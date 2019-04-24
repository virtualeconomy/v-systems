package vsys.contract

import com.google.common.primitives.{Chars, Shorts}
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Random

trait OpcFunction extends FunId with ProtoType with ListOpc {

  def aFunctionGen(funIdx: Gen[Array[Byte]], protoTypeGen: Gen[Array[Byte]], listOpcGen: Gen[Array[Byte]]): Gen[Array[Byte]] = for {
    funId <- funIdx
    protoType <- protoTypeGen
    listOpc <- listOpcGen
    fun <- Gen.const(funId.array ++ protoType.array ++ listOpc.array)
  } yield fun

  def aFunctionRandomGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(randomFunIdGen, protoTypeRandomGen, listOpcLinesRandom)
  } yield fun

  def descriptorFullGen(): Gen[Seq[Array[Byte]]] = for {
    supersede <- supersedeFunGen()
    issue <- issueFunGen()
    destroy <- destroyFunGen()
    split <- splitFunGen()
    send <- sendFunGen()
    transfer <- transferFunGen()
    deposit <- depositFunGen()
    withdraw <- withdrawFunGen()
    totalSupply <- totalSupplyFunGen()
    maxSupply <- maxSupplyFunGen()
    balanceOf <- balanceOfFunGen()
    getIssuer <- getIssuerFunGen()
  } yield Seq(supersede, issue, destroy, split, send, transfer, deposit, withdraw, totalSupply, maxSupply, balanceOf, getIssuer)

  def descriptorRandomGen(): Gen[Seq[Array[Byte]]] = for {
    descriptor <- aFunctionRandomGen()
  } yield Seq(descriptor)

  def initFunWrongParaGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(initFunIdGen, protoTypeInitWrongGen, initOpcLineGen)
  } yield fun

  def initWrongTDBFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(initFunIdGen, protoTypeInitGen, initOpcLineWrongTDBGen)
  } yield fun

  def initFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(initFunIdGen, protoTypeInitGen, initOpcLineGen)
  } yield fun

  def supersedeFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(supersedeFunIdGen, protoTypeSupersedeGen, supersedeOpcLineGen)
  } yield fun

  def issueFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(issueFunIdGen, protoTypeIssueGen, issueOpcLineGen)
  } yield fun

  def destroyFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(destroyFunIdGen, protoTypeDestroyGen, destroyOpcLineGen)
  } yield fun

  def splitFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(splitFunIdGen, protoTypeSplitGen, splitOpcLineGen)
  } yield fun

  def sendFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(sendFunIdGen, protoTypeSendGen, sendOpcLineGen)
  } yield fun

  def transferFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(transferFunIdGen, protoTypeTransferGen, transferOpcLineGen)
  } yield fun

  def depositFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(depositFunIdGen, protoTypeDepositGen, depositOpcLineGen)
  } yield fun

  def withdrawFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(withdrawFunIdGen, protoTypeWithdrawGen, withdrawOpcLineGen)
  } yield fun

  def totalSupplyFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(totalSupplyFunIdGen, protoTypeTotalSupplyGen, totalSupplyOpcLineGen)
  } yield fun

  def maxSupplyFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(maxSupplyFunIdGen, protoTypeMaxSupplyGen, maxSupplyOpcLineGen)
  } yield fun

  def balanceOfFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(balanceOfFunIdGen, protoTypeBalanceOfGen, balanceOfOpcLineGen)
  } yield fun

  def getIssuerFunGen(): Gen[Array[Byte]] = for {
    fun <- aFunctionGen(getIssuerFunIdGen, protoTypeGetIssuerGen, getIssuerOpcLineGen)
  } yield fun
}

trait FunId {
  import FunId._
  def funIdxGen(id: Short): Gen[Array[Byte]] = for {
    funId <- Gen.const(Shorts.toByteArray(id))
  } yield funId

  val randomFunIdGen: Gen[Array[Byte]] = funIdxGen(randomFunId)
  val initFunIdGen: Gen[Array[Byte]] = funIdxGen(init)
  val supersedeFunIdGen: Gen[Array[Byte]] = funIdxGen(supersede)
  val issueFunIdGen: Gen[Array[Byte]] = funIdxGen(issue)
  val destroyFunIdGen: Gen[Array[Byte]] = funIdxGen(destroy)
  val splitFunIdGen: Gen[Array[Byte]] = funIdxGen(split)
  val sendFunIdGen: Gen[Array[Byte]] = funIdxGen(send)
  val transferFunIdGen: Gen[Array[Byte]] = funIdxGen(transfer)
  val depositFunIdGen: Gen[Array[Byte]] = funIdxGen(deposit)
  val withdrawFunIdGen: Gen[Array[Byte]] = funIdxGen(withdraw)
  val totalSupplyFunIdGen: Gen[Array[Byte]] = funIdxGen(totalSupply)
  val maxSupplyFunIdGen: Gen[Array[Byte]] = funIdxGen(maxSupply)
  val balanceOfFunIdGen: Gen[Array[Byte]] = funIdxGen(balanceOf)
  val getIssuerFunIdGen: Gen[Array[Byte]] = funIdxGen(getIssuer)
}

object FunId {
  val randomFunId: Short = Random.nextInt(1 << 15).toShort
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

  val supersedeIndex: Short = 0
  val issueIndex: Short = 1
  val destroyIndex: Short = 2
  val splitIndex: Short = 3
  val sendIndex: Short = 4
  val transferIndex: Short = 5
  val depositIndex: Short = 6
  val withdrawIndex: Short = 7
  val totalSupplyIndex: Short = 8
  val maxSupplyIndex: Short = 9
  val balanceOfIndex: Short = 10
  val getIssuerIndex: Short = 11
}

trait ProtoType {
  import ProtoType._
  def protoTypeGen(returnType: Byte, listParaTypes: List[Byte]): Gen[Array[Byte]] = for {
    len <- Gen.const(Shorts.toByteArray((listParaTypes.length + 1).toShort))
    retType <- Gen.const(Array(returnType))
    listPT <- Gen.const(listParaTypes.toArray)
    protoType <- Gen.const(len.array ++ retType.array ++ listPT.array)
  } yield protoType

  val returnType: Byte = 0
  val protoTypeInitWrongGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaTypeWrong)
  val protoTypeInitGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeSupersedeGen: Gen[Array[Byte]] = protoTypeGen(returnType, supersedeParaType)
  val protoTypeIssueGen: Gen[Array[Byte]] = protoTypeGen(returnType, issueParaType)
  val protoTypeDestroyGen: Gen[Array[Byte]] = protoTypeGen(returnType, destroyParaType)
  val protoTypeSplitGen: Gen[Array[Byte]] = protoTypeGen(returnType, splitParaType)
  val protoTypeSendGen: Gen[Array[Byte]] = protoTypeGen(returnType, sendParaType)
  val protoTypeTransferGen: Gen[Array[Byte]] = protoTypeGen(returnType, transferParaType)
  val protoTypeDepositGen: Gen[Array[Byte]] = protoTypeGen(returnType, depositParaType)
  val protoTypeWithdrawGen: Gen[Array[Byte]] = protoTypeGen(returnType, withdrawParaType)
  val protoTypeTotalSupplyGen: Gen[Array[Byte]] = protoTypeGen(DataType.Amount.id.toByte, totalSupplyParaType)
  val protoTypeMaxSupplyGen: Gen[Array[Byte]] = protoTypeGen(DataType.Amount.id.toByte, maxSupplyParaType)
  val protoTypeBalanceOfGen: Gen[Array[Byte]] = protoTypeGen(DataType.Amount.id.toByte, balanceOfParaType)
  val protoTypeGetIssuerGen: Gen[Array[Byte]] = protoTypeGen(DataType.Account.id.toByte, getIssuerParaType)

  private val minParaTypeSize: Short = 2
  private val maxParaTypeSize: Short = 128

  val protoTypeRandomGen: Gen[Array[Byte]] = for {
    length <- Gen.chooseNum(minParaTypeSize, maxParaTypeSize)
    returnType <- Gen.numChar
    listParaTypes <- Gen.listOfN(length - 1, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Shorts.toByteArray(length) ++ Chars.toByteArray(returnType) ++ listParaTypes
}

object ProtoType {
  val initParaTypeWrong: List[Byte] = List(DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val initParaType: List[Byte] = List(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)
  val supersedeParaType: List[Byte] = List(DataType.Account.id.toByte)
  val issueParaType: List[Byte] = List(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val destroyParaType: List[Byte] = List(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val splitParaType: List[Byte] = List(DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val sendParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val transferParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val depositParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  // val depositParaType: List[Byte] = List(DataType.Address.id.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val withdrawParaType: List[Byte] = List(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  //val withdrawParaType: List[Byte] = List(DataType.Address.id.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.Int32.id.toByte)
  val totalSupplyParaType: List[Byte] = List(DataType.Int32.id.toByte)
  val maxSupplyParaType: List[Byte] = List(DataType.Int32.id.toByte)
  val balanceOfParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.Int32.id.toByte)
  val getIssuerParaType: List[Byte] = List.empty
}

trait ListOpc extends ByteArrayGen {
  import ListOpc._

  def listOpcGen(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Gen[Array[Byte]] = for {
    length <- Gen.const(Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort))
    numOpc <- Gen.const(Shorts.toByteArray(ids.length.toShort))
    listOpc <- Gen.const(ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten) //(x._1 ++ x._2).length.toShort ++
    lenListOpc <- Gen.const(length.array ++ numOpc ++ listOpc.array)
  } yield lenListOpc

  val initOpcLineWrongTDBGen: Gen[Array[Byte]] = listOpcGen(initWrongTDBOpc, initOpcIndex)
  val initOpcLineGen: Gen[Array[Byte]] = listOpcGen(initOpc, initOpcIndex)
  val supersedeOpcLineGen: Gen[Array[Byte]] = listOpcGen(supersedeOpc, supersedeOpcIndex)
  val issueOpcLineGen: Gen[Array[Byte]] = listOpcGen(issueOpc, issueOpcIndex)
  val destroyOpcLineGen: Gen[Array[Byte]] = listOpcGen(destroyOpc, destroyOpcIndex)
  val splitOpcLineGen: Gen[Array[Byte]] = listOpcGen(splitOpc, splitOpcIndex)
  val sendOpcLineGen: Gen[Array[Byte]] = listOpcGen(sendOpc, sendOpcIndex)
  val transferOpcLineGen: Gen[Array[Byte]] = listOpcGen(transferOpc, transferOpcIndex)
  val depositOpcLineGen: Gen[Array[Byte]] = listOpcGen(depositOpc, depositOpcIndex)
  val withdrawOpcLineGen: Gen[Array[Byte]] = listOpcGen(withdrawOpc, withdrawOpcIndex)
  val totalSupplyOpcLineGen: Gen[Array[Byte]] = listOpcGen(totalSupplyOpc, totalSupplyOpcIndex)
  val maxSupplyOpcLineGen: Gen[Array[Byte]] = listOpcGen(maxSupplyOpc, maxSupplyOpcIndex)
  val balanceOfOpcLineGen: Gen[Array[Byte]] = listOpcGen(balanceOfOpc, balanceOfOpcIndex)
  val getIssuerOpcLineGen: Gen[Array[Byte]] = listOpcGen(getIssuerOpc, getIssuerOpcIndex)

  val listOpcLinesRandom: Gen[Array[Byte]] = for {
    listOpcLines <- byteArrayRandomGen()
  } yield listOpcLines
}

object ListOpc {
  val opcLoadSignerIndex: Array[Byte] = Array(3.toByte)
  val opcLoadCallerIndex: Array[Byte] = Array(3.toByte)

  val initOpcCDBVSetSignerIndex: Array[Byte] = Array(StateVar.issuer, DataStack.initInput.issuerLoadIndex)
  val initOpcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)
  val initOpcTDBNewTokenIndex: Array[Byte] = Array(DataStack.initInput.maxIndex, DataStack.initInput.unityIndex, DataStack.initInput.shortTextIndex)
  val initWrongTDBOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet, Array(5.toByte, 3.toByte))
  val initOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet, OpcId.opcTDBNewToken)
  val initOpcIndex: List[Array[Byte]] = List(opcLoadSignerIndex, initOpcCDBVSetSignerIndex, initOpcCDBVSetMakerIndex, initOpcTDBNewTokenIndex)

  val supersedeOpcCDBVRGetIndex: Array[Byte] = Array(StateVar.maker, 1.toByte)
  val superAssertIsSignerOriginIndex: Array[Byte] = Array(DataStack.supersedeInput.maker)
  val supersedeOpcCDBVSetIndex: Array[Byte] = Array(StateVar.issuer, DataStack.supersedeInput.newIssuerIndex)
  val supersedeOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsSignerOrigin, OpcId.opcCDBVSet)
  val supersedeOpcIndex: List[Array[Byte]] = List(supersedeOpcCDBVRGetIndex, superAssertIsSignerOriginIndex, supersedeOpcCDBVSetIndex)


  val issueOpcCDBVRGetIndex: Array[Byte] = Array(StateVar.issuer, 2.toByte)
  val issueOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex)
  val issueOpcTDBADepositIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex, DataStack.issueInput.amountIndex, DataStack.issueInput.tokenIndex)
  val issueOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBADeposit)
  val issueOpcIndex: List[Array[Byte]] = List(issueOpcCDBVRGetIndex, issueOpcAssertIsCallerOriginIndex, issueOpcTDBADepositIndex)

  val destroyOpcCDBVRGetIndex: Array[Byte] = Array(StateVar.issuer, 2.toByte)
  val destroyOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.destroyInput.issuerGetIndex)
  val destroyOpcTDBAWithdrawIndex: Array[Byte] = Array(DataStack.destroyInput.issuerGetIndex, DataStack.destroyInput.destroyAmountIndex,
    DataStack.destroyInput.tokenIndex)
  val destroyOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBAWithdraw)
  val destroyOpcIndex: List[Array[Byte]] = List(destroyOpcCDBVRGetIndex, destroyOpcAssertIsCallerOriginIndex, destroyOpcTDBAWithdrawIndex)

  val splitOpcCDBVRGetIndex: Array[Byte] = Array(StateVar.issuer, 2.toByte)
  val splitOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.splitInput.issuerGetIndex)
  val splitOpcTDBSplitIndex: Array[Byte] = Array(DataStack.splitInput.newUnityIndex, DataStack.splitInput.tokenIndex)
  val splitOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBSplit)
  val splitOpcIndex: List[Array[Byte]] = List(splitOpcCDBVRGetIndex, splitOpcAssertIsCallerOriginIndex, splitOpcTDBSplitIndex)

  val sendOpcTDBATransferIndex: Array[Byte] = Array(DataStack.sendInput.senderIndex,
    DataStack.sendInput.recipientIndex, DataStack.sendInput.amountIndex, DataStack.sendInput.tokenIndex)
  val sendOpc: List[Array[Byte]] = List(OpcId.opcLoadCaller, OpcId.opcTDBATransfer)
  val sendOpcIndex: List[Array[Byte]] = List(opcLoadCallerIndex, sendOpcTDBATransferIndex)

  val transferOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex)
  val transferOpcTDBATransferIndex: Array[Byte] = Array(DataStack.transferInput.senderIndex, DataStack.transferInput.recipientIndex,
    DataStack.transferInput.amountIndex, DataStack.transferInput.tokenIndex)
  val transferOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
  val transferOpcIndex: List[Array[Byte]] = List(transferOpcAssertIsCallerOriginIndex, transferOpcTDBATransferIndex)

  val depositOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex)
  val depositOpcTDBATransferIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex, DataStack.depositInput.smartContractIndex,
    DataStack.depositInput.amountIndex, DataStack.depositInput.tokenIndex)
  val depositOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
  val depositOpcIndex: List[Array[Byte]] = List(depositOpcAssertIsCallerOriginIndex, depositOpcTDBATransferIndex)

  val withdrawOpcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.withdrawInput.recipientIndex)
  val withdrawOpcTDBATransferIndex: Array[Byte] = Array(DataStack.withdrawInput.smartContractIndex, DataStack.withdrawInput.recipientIndex,
    DataStack.withdrawInput.amountIndex, DataStack.withdrawInput.tokenIndex)
  val withdrawOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
  val withdrawOpcIndex: List[Array[Byte]] = List(withdrawOpcAssertIsCallerOriginIndex, withdrawOpcTDBATransferIndex)

  val totalSupplyOpcTDBRTotalIndex: Array[Byte] = Array(DataStack.totalSupplyInput.tokenIndex, 1.toByte)
  val totalSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcTotal, OpcId.opcReturnValue)
  val totalSupplyOpcIndex: List[Array[Byte]] = List(totalSupplyOpcTDBRTotalIndex, Array(1.toByte))

  val maxSupplyOpcTDBRMaxIndex: Array[Byte] = Array(DataStack.maxSupplyInput.tokenIndex, 1.toByte)
  val maxSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcMax, OpcId.opcReturnValue)
  val maxSupplyOpcIndex: List[Array[Byte]] = List(maxSupplyOpcTDBRMaxIndex, Array(1.toByte))

  val balanceOfOpcTDBARBalanceIndex: Array[Byte] = Array(DataStack.balanceOfInput.accountIndex, DataStack.balanceOfInput.tokenIndex, 2.toByte)
  val balanceOfOpc: List[Array[Byte]] = List(OpcId.opcTDBARBalance, OpcId.opcReturnValue)
  val balanceOfOpcIndex: List[Array[Byte]] = List(balanceOfOpcTDBARBalanceIndex, Array(2.toByte))

  val getIssuerOpcCDBVRGetIndex: Array[Byte] = Array(StateVar.issuer, 0.toByte)
  val getIssuerOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcReturnValue)
  val getIssuerOpcIndex: List[Array[Byte]] = List(getIssuerOpcCDBVRGetIndex, Array(0.toByte))
}
