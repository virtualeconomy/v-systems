package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Shorts}
import vsys.utils.serialization.Deser

object ContractGen {

  val sysTransfer              = Array(0.toByte, 1.toByte)

  val assertSig                = Array(1.toByte, 10.toByte)
  val assertHash               = Array(1.toByte, 9.toByte)
  val assertTrue               = Array(1.toByte, 8.toByte)
  val assertSigner             = Array(1.toByte, 7.toByte)
  val assertCaller             = Array(1.toByte, 6.toByte)
  val assertEqual              = Array(1.toByte, 5.toByte)

  val loadSigner               = Array(2.toByte, 1.toByte)
  val loadCaller               = Array(2.toByte, 2.toByte)
  val loadTimestamp            = Array(2.toByte, 3.toByte)
  val loadLastTokenIndex       = Array(2.toByte, 4.toByte)
  val loadTransactionId        = Array(2.toByte, 5.toByte)
  val loadPublicKey            = Array(2.toByte, 6.toByte)

  val cdbvSet                  = Array(3.toByte, 1.toByte)
  val cdbvMapSet               = Array(3.toByte, 2.toByte)
  val cdbvMapValAdd            = Array(3.toByte, 3.toByte)
  val cdbvMapValMinus          = Array(3.toByte, 4.toByte)
  val cdbvStateValAdd          = Array(3.toByte, 5.toByte)
  val cdbvStateValMinus        = Array(3.toByte, 6.toByte)

  val cdbvrGet                 = Array(4.toByte, 1.toByte)
  val cdbvrMapGetOrDefault     = Array(4.toByte, 2.toByte)
  val cdbvrMapGet              = Array(4.toByte, 3.toByte)
  val cdbvrGetOrDefault        = Array(4.toByte, 4.toByte)

  val tdbNewToken              = Array(5.toByte, 1.toByte)
  val tdbSplit                 = Array(5.toByte, 2.toByte)

  val tdbroOpcMax              = Array(6.toByte, 1.toByte)
  val tdbroOpcTotal            = Array(6.toByte, 2.toByte)

  val tdbaDeposit              = Array(7.toByte, 1.toByte)
  val tdbaWithdraw             = Array(7.toByte, 2.toByte)
  val tdbaTransfer             = Array(7.toByte, 3.toByte)

  val tdbarBalance             = Array(8.toByte, 1.toByte)

  val returnValue              = Array(9.toByte, 1.toByte)

  val compareGreaterEqual      = Array(10.toByte, 1.toByte)
  val compareGreater           = Array(10.toByte, 2.toByte)

  val basicAdd                 = Array(11.toByte, 1.toByte)
  val basicMinus               = Array(11.toByte, 2.toByte)
  val basicMultiply            = Array(11.toByte, 3.toByte)
  val basicDivide              = Array(11.toByte, 4.toByte)
  val basicMin                 = Array(11.toByte, 5.toByte)
  val basicMax                 = Array(11.toByte, 6.toByte)
  val basicConcat              = Array(11.toByte, 7.toByte)
  val basicConstantGet         = Array(11.toByte, 8.toByte)
  val basicSqrtBigint          = Array(11.toByte, 9.toByte)
  val basicConvert             = Array(11.toByte, 10.toByte)

  sealed case class StateVar(index: Byte, dataType: Byte) {
    lazy val arr: Array[Byte] = Array(index, dataType)
  }

  sealed case class StateMap(index: Byte, keyType: Byte, valueType: Byte) {
    lazy val arr: Array[Byte] = Array(index, keyType, valueType)
  }

  def genFunctionOpcs(opcs: Seq[Array[Byte]]): Array[Byte] = {
    val numOpc = Shorts.toByteArray(opcs.length.toShort)
    val combinedOpcs = opcs.map(x => Deser.serializeArray(x)).toArray.flatten
    val length = Shorts.toByteArray((combinedOpcs.length + 2).toShort)
    Bytes.concat(length, numOpc, combinedOpcs)
  }

  def getFunctionBytes(id: Short, funcType: Byte, retType: Array[Byte],
                       listParaTypes: Array[Byte], opcLines: Seq[Array[Byte]]): Array[Byte] =
    Shorts.toByteArray(id) ++ Array(funcType) ++ protoType(retType, listParaTypes) ++ genFunctionOpcs(opcLines)

  def protoType(listReturnType: Array[Byte], listParaTypes: Array[Byte]): Array[Byte] = {
    val retType = Deser.serializeArray(listReturnType)
    val paraType = Deser.serializeArray(listParaTypes)
    Bytes.concat(retType, paraType)
  }

  def textualFunc(name: String, ret: Seq[String], para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x))))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  def textualStateMap(a: Seq[Seq[String]]): Array[Byte] = {
    Deser.serializeArrays(a.map(x => Deser.serializeArrays(x.map(y => Deser.serilizeString(y)))))
  }

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0
  val onDepositTriggerType: Byte = 1
  val onWithDrawTriggerType: Byte = 2
  val publicFuncType: Byte = 0

}
