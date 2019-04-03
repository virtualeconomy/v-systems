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
    init <- initFunGen()
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
  } yield Seq(init, supersede, issue, destroy, split, send, transfer, deposit, withdraw, totalSupply, maxSupply, balanceOf, getIssuer)

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

trait ProtoType {
  import ProtoType._
  def protoTypeGen(returnType: Byte, listParaTypes: List[Byte]): Gen[Array[Byte]] = for {
    len <- Gen.const(Shorts.toByteArray((listParaTypes.length + 1).toShort))
    retType <- Gen.const(Array(returnType))
    listPT <- Gen.const(listParaTypes.toArray)
    protoType <- Gen.const(len.array ++ retType.array ++ listPT.array)
  } yield protoType

  val returnType: Byte = 1
  val protoTypeInitWrongGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaTypeWrong)
  val protoTypeInitGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeSupersedeGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeIssueGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeDestroyGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeSplitGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeSendGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeTransferGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeDepositGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeWithdrawGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeTotalSupplyGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeMaxSupplyGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeBalanceOfGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)
  val protoTypeGetIssuerGen: Gen[Array[Byte]] = protoTypeGen(returnType, initParaType)

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
  val issueParaType: List[Byte] = List(DataType.Amount.id.toByte)
  val destroyParaType: List[Byte] = List(DataType.Amount.id.toByte)
  val splitParaType: List[Byte] = List(DataType.Amount.id.toByte)
  val sendParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val transferParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val depositParaType: List[Byte] = List(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)
  val withdrawParaType: List[Byte] = List(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val totalSupplyParaType: List[Byte] = List()
  val maxSupplyParaType: List[Byte] = List()
  val balanceOfParaType: List[Byte] = List(DataType.Account.id.toByte)
  val getIssuerParaType: List[Byte] = List()
}

trait ListOpc extends ByteArrayGen {
  import ListOpc._

  def listOpcGen(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Gen[Array[Byte]] = for {
    length <- Gen.const(Shorts.toByteArray(ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 4).toShort).sum))
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
  val opcLoadSignerIndex: Array[Byte] = Array()
  val opcLoadCallerIndex: Array[Byte] = Array()

  val opcCDBVSetIssuerIndex: Array[Byte] = Array(StateVar.issuer, DataStack.initInput.issuerLoadIndex)
  val opcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)
  val opcTDBNewTokenIndex: Array[Byte] = Array(StateVar.max, StateVar.total, StateVar.unity, StateVar.shortText,
    DataStack.initInput.maxIndex, DataStack.initInput.unityIndex)

  val initWrongTDBOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet, Array(5.toByte, 3.toByte))
  val initOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet, OpcId.opcTDBNewToken)
  val initOpcIndex: List[Array[Byte]] = List(opcLoadSignerIndex, opcCDBVSetIssuerIndex, opcCDBVSetMakerIndex, opcTDBNewTokenIndex)

  // supersede index and opc
  val supersedeOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsSignerOrigin, OpcId.opcCDBVSet)
  val supersedeOpcIndex: List[Array[Byte]] = List(Array())


  val opcCDBVRGetIndex: Array[Byte] = Array(StateVar.issuer, DataStack.issueInput.issuerGetIndex)
  val opcAssertIsCallerOriginIndex: Array[Byte] = Array(DataStack.issueInput.issuerGetIndex)
  val opcTDBADepositIndex: Array[Byte] = Array(StateVar.max, StateVar.total,
    DataStack.issueInput.issuerGetIndex, DataStack.issueInput.amountIndex, DataStack.issueInput.totalIndex)
  val issueOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBADeposit)
  val issueOpcIndex: List[Array[Byte]] = List(opcCDBVRGetIndex,opcAssertIsCallerOriginIndex, opcTDBADepositIndex)

  // destroy index and opc
  val destroyOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBAWithdraw)
  val destroyOpcIndex: List[Array[Byte]] = List(Array())

  // split
  val splitOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBSplit)
  val splitOpcIndex: List[Array[Byte]] = List(Array())

  // send
  val sendOpc: List[Array[Byte]] = List(OpcId.opcLoadCaller, OpcId.opcTDBATransfer)
  val sendOpcIndex: List[Array[Byte]] = List(Array())

  // transfer
  val transferOpc: List[Array[Byte]] = List(OpcId.opcAssertIsSignerOrigin, OpcId.opcTDBATransfer)
  val transferOpcIndex: List[Array[Byte]] = List(Array())

  //deposit
  val depositOpc: List[Array[Byte]] = List(OpcId.opcAssertIsSignerOrigin, OpcId.opcTDBATransfer)
  val depositOpcIndex: List[Array[Byte]] = List(Array())

  // withdraw
  val withdrawOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcTDBATransfer)
  val withdrawOpcIndex: List[Array[Byte]] = List(Array())

  // totalSupply
  val totalSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcTotal, OpcId.opcReturnValue)
  val totalSupplyOpcIndex: List[Array[Byte]] = List(Array())

  // maxSupplyOpc
  val maxSupplyOpc: List[Array[Byte]] = List(OpcId.opcTDBROpcGet, OpcId.opcReturnValue)
  val maxSupplyOpcIndex: List[Array[Byte]] = List(Array())

  // balanceOfOpc
  val balanceOfOpc: List[Array[Byte]] = List(OpcId.opcTDBARBalance, OpcId.opcReturnValue)
  val balanceOfOpcIndex: List[Array[Byte]] = List(Array())

  //getIssuerOpc
  val getIssuerOpc: List[Array[Byte]] = List(OpcId.opcCDBVRGet, OpcId.opcReturnValue)
  val getIssuerOpcIndex: List[Array[Byte]] = List(Array())
}


