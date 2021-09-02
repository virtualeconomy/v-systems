package vsys.blockchain.contract

import org.scalacheck.Gen
import com.google.common.primitives.{Bytes, Ints, Shorts}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.state._
import vsys.blockchain.transaction.TransactionGen
import vsys.utils.serialization.Deser

object ContractGenHelper extends TransactionGen {

  val assertTrue               = Array(1.toByte, 8.toByte)
  val assertSigner             = Array(1.toByte, 7.toByte)
  val assertCaller             = Array(1.toByte, 6.toByte)
  val assertEqual              = Array(1.toByte, 5.toByte)

  val loadSigner               = Array(2.toByte, 1.toByte)
  val loadCaller               = Array(2.toByte, 2.toByte)
  val loadTimestamp            = Array(2.toByte, 3.toByte)

  val cdbvSet                  = Array(3.toByte, 1.toByte)
  val cdbvMapSet               = Array(3.toByte, 2.toByte)
  val cdbvMapValAdd            = Array(3.toByte, 3.toByte)
  val cdbvMapValMinus          = Array(3.toByte, 4.toByte)

  val cdbvrGet                 = Array(4.toByte, 1.toByte)
  val cdbvrMapGetOrDefault     = Array(4.toByte, 2.toByte)

  val tdbNewToken              = Array(5.toByte, 1.toByte)
  val tdbSplit                 = Array(5.toByte, 2.toByte)

  val tdbrMax                  = Array(6.toByte, 1.toByte)
  val tdbrTotal                = Array(6.toByte, 2.toByte)
  val tdbrUnity                = Array(6.toByte, 3.toByte)
  val tdbrDesc                 = Array(6.toByte, 4.toByte)

  val tdbaMint                 = Array(7.toByte, 1.toByte)
  val tdbaBurn                 = Array(7.toByte, 2.toByte)
  val tdbaTransfer             = Array(7.toByte, 3.toByte)

  val tdbarBalance             = Array(8.toByte, 1.toByte)

  val returnValue              = Array(9.toByte, 1.toByte)

  val compareGreaterEqual           = Array(10.toByte, 1.toByte)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val feeScale: Short = 100

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

  def dataListGen(seqDataByte: Seq[Array[Byte]], seqDataType: Seq[DataType.DataTypeVal[_]]): Gen[Seq[DataEntry]] =
    seqDataByte.zip(seqDataType).map { case (e1: Array[Byte], e2: DataType.DataTypeVal[_]) => DataEntry.create(e1, e2).explicitGet()}

  def basicContractTestGen(): Gen[(PrivateKeyAccount, Long, Long)] = for {
    master <- accountGen
    timestamp <- positiveLongGen
    fee <- smallFeeGen
  } yield (master, timestamp, fee)

  def languageCodeGen(code: String): Gen[Array[Byte]] = for {
    languageCode <- Gen.const(Deser.serilizeString(code))
  } yield languageCode

  def languageVersionGen(version: Int): Gen[Array[Byte]] = for {
    languageVersion <- Gen.const(Ints.toByteArray(version))
  } yield languageVersion

  def contractNewGen(languageCode: String, languageVersion: Int, init: Gen[Seq[Array[Byte]]],
                     desc: Gen[Seq[Array[Byte]]], state: Gen[Seq[Array[Byte]]], text: Gen[Seq[Array[Byte]]]): Gen[Contract] = for {
    langCode <- languageCodeGen(languageCode)
    langVer <- languageVersionGen(languageVersion)
    initializer <- init
    descriptor <- desc
    stateVar <- state
    textual <- text
  } yield Contract.buildContract(langCode, langVer, initializer, descriptor, stateVar, Seq(), textual).explicitGet()

}
