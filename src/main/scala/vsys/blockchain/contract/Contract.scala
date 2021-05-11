package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Shorts}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.InvalidContract
import vsys.utils.ScorexLogging
import vsys.utils.base58Length
import vsys.utils.serialization.Deser

import scala.util.{Success, Try}

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + Base58.encode(languageCode) + ":" + Base58.encode(languageVersion)
  lazy val bytes: ByteStr = if (languageVersion sameElements Ints.toByteArray(1))
                           ByteStr(languageCode ++ languageVersion
                             ++ Deser.serializeArray(Deser.serializeArrays(trigger))
                             ++ Deser.serializeArray(Deser.serializeArrays(descriptor))
                             ++ Deser.serializeArray(Deser.serializeArrays(stateVar))
                             ++ Deser.serializeArrays(textual))
                         else ByteStr(languageCode ++ languageVersion
                                ++ Deser.serializeArray(Deser.serializeArrays(trigger))
                                ++ Deser.serializeArray(Deser.serializeArrays(descriptor))
                                ++ Deser.serializeArray(Deser.serializeArrays(stateVar))
                                ++ Deser.serializeArray(Deser.serializeArrays(stateMap))
                                ++ Deser.serializeArrays(textual))

  val trigger: Seq[Array[Byte]]
  val descriptor: Seq[Array[Byte]]
  val stateVar: Seq[Array[Byte]]
  val stateMap: Seq[Array[Byte]]
  val textual: Seq[Array[Byte]]
  val languageCode: Array[Byte]
  val languageVersion: Array[Byte]

  lazy val json: JsObject = if (languageVersion sameElements Ints.toByteArray(1))
    Json.obj(
    "languageCode" -> Deser.deserilizeString(languageCode),
    "languageVersion" -> Ints.fromByteArray(languageVersion),
    "triggers" -> trigger.map(p => Base58.encode(p)),
    "descriptors" -> descriptor.map(p => Base58.encode(p)),
    "stateVariables" -> stateVar.map(p => Base58.encode(p)),
    "textual" -> Json.obj("triggers" -> Base58.encode(textual(0)),
      "descriptors" -> Base58.encode(textual(1)),
      "stateVariables" -> Base58.encode(textual(2)))
    )
  else Json.obj(
    "languageCode" -> Deser.deserilizeString(languageCode),
    "languageVersion" -> Ints.fromByteArray(languageVersion),
    "triggers" -> trigger.map(p => Base58.encode(p)),
    "descriptors" -> descriptor.map(p => Base58.encode(p)),
    "stateVariables" -> stateVar.map(p => Base58.encode(p)),
    "stateMaps" -> stateMap.map(p => Base58.encode(p)),
    "textual" -> Json.obj("triggers" -> Base58.encode(textual(0)),
      "descriptors" -> Base58.encode(textual(1)),
      "stateVariables" -> Base58.encode(textual(2)),
    "stateMaps" -> Base58.encode(textual(3)))
  )

  override def equals(obj: Any): Boolean = obj match {
    case a: Contract => bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)

}

object Contract extends ScorexLogging {

  val Prefix: String = "contract:"

  val MinContractByteSize = 8
  val MinContractStringSize: Int = base58Length(MinContractByteSize)
  val LanguageCodeByteLength = 4
  val LanguageVersionByteLength = 4
  val LanguageCodeByte: Array[Byte] = Deser.serilizeString("vdds")
  val LanguageVersionByte: Array[Byte] = Ints.toByteArray(1)
  val validContractBytesList: Seq[Array[Byte]] = Seq(
    ContractPermitted.contract.bytes.arr,
    ContractPermitted.contractWithoutSplit.bytes.arr,
    ContractLock.contract.bytes.arr,
    ContractNonFungible.contract.bytes.arr,
    ContractPaymentChannel.contract.bytes.arr,
    ContractAtomicSwap.contract.bytes.arr,
    ContractVSwap.contract.bytes.arr,
    ContractVOption.contract.bytes.arr,
    ContractVStableSwap.contract.bytes.arr,
    ContractVEscrow.contract.bytes.arr,
    ContractTokenV2.contractTokenWhiteList.bytes.arr,
    ContractTokenV2.contractTokenBlackList.bytes.arr,
    ContractNonFungibleV2.contractNFTWhitelist.bytes.arr,
    ContractNonFungibleV2.contractNFTBlacklist.bytes.arr
  )

  def buildContract(languageCode: Array[Byte], languageVersion: Array[Byte],
                    trigger: Seq[Array[Byte]], descriptor: Seq[Array[Byte]],
                    stateVar: Seq[Array[Byte]], stateMap: Seq[Array[Byte]],
                    textual: Seq[Array[Byte]]): Either[ValidationError, Contract] = {
    case class ContractImpl(languageCode: Array[Byte], languageVersion: Array[Byte],
                            trigger: Seq[Array[Byte]], descriptor: Seq[Array[Byte]],
                            stateVar: Seq[Array[Byte]], stateMap: Seq[Array[Byte]],
                            textual: Seq[Array[Byte]]) extends Contract
    Right(ContractImpl(languageCode, languageVersion, trigger, descriptor, stateVar, stateMap, textual))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    val contract = Try {
      val languageCode = bytes.slice(0, LanguageCodeByteLength)
      val languageVersion = bytes.slice(LanguageCodeByteLength, LanguageCodeByteLength + LanguageVersionByteLength)
      val (triggerBytes, triggerEnd) = Deser.parseArraySize(bytes, LanguageCodeByteLength + LanguageVersionByteLength)
      val trigger = Deser.parseArrays(triggerBytes)
      val (descriptorBytes, descriptorEnd) = Deser.parseArraySize(bytes, triggerEnd)
      val descriptor = Deser.parseArrays(descriptorBytes)
      val (stateVarBytes, stateVarEnd) = Deser.parseArraySize(bytes, descriptorEnd)
      val stateVar = Deser.parseArrays(stateVarBytes)

      val (lastBytes, lastEnd) =
        if (languageVersion sameElements Ints.toByteArray(1))
          (stateVarBytes, stateVarEnd)
        else Deser.parseArraySize(bytes, stateVarEnd)

      val stateMap =
        if (languageVersion sameElements Ints.toByteArray(1))
          Deser.parseArrays(Shorts.toByteArray(0))
        else Deser.parseArrays(lastBytes)

      val textual = Deser.parseArrays(bytes.slice(lastEnd, bytes.length))
      if (isByteArrayValid(bytes, textual)){
        buildContract(languageCode, languageVersion, trigger, descriptor, stateVar, stateMap, textual)
      } else {
        Left(InvalidContract)
      }
    }
    contract.getOrElse(Left(InvalidContract))
  }

  def fromBase58String(base58String: String): Either[ValidationError, Contract] = {
    if (base58String.length < MinContractStringSize) Left(InvalidContract)
    else {
      Base58.decode(base58String) match {
        case Success(byteArray) => fromBytes(byteArray)
        case _ => Left(InvalidContract)
      }
    }
  }

  def checkStateVar(stateVar: Array[Byte]): Boolean =
    stateVar.length == 2

  def checkStateVar(stateVar: Array[Byte], dataType: DataType.DataTypeVal[_]): Boolean =
    stateVar.length == 2 && DataType.check(dataType.id.toByte, stateVar(1))

  def checkStateMap(stateMap: Array[Byte], keyDataType: DataType.DataTypeVal[_]): Boolean =
    stateMap.length == 3 && DataType.check(keyDataType.id.toByte, stateMap(1))

  def checkStateMap(stateMap: Array[Byte], keyDataType: DataType.DataTypeVal[_], valueDataType: DataType.DataTypeVal[_]): Boolean =
    stateMap.length == 3 && DataType.check(keyDataType.id.toByte, stateMap(1)) && DataType.check(valueDataType.id.toByte, stateMap(2))

  private def checkContractBytes(bytes: Array[Byte]): Boolean = {
    validContractBytesList.forall(y => !(bytes sameElements y))
  }

  private def isByteArrayValid(bytes: Array[Byte], textual: Seq[Array[Byte]]): Boolean = {
    val textualStr = textualFromBytes(textual)
    if (checkContractBytes(bytes)) {
      log.warn(s"Illegal contract ${bytes.mkString(" ")}")
      false
    } else if (textualStr.isFailure ||
      !checkTextual(textualStr.getOrElse((Seq.empty[Seq[String]], Seq.empty[Seq[String]], Seq.empty[String], Seq.empty[Seq[String]])))) {
      log.warn(s"Illegal textual ${textual.mkString(" ")}")
      false
    } else true
  }

  private def textualFromBytes(bs: Seq[Array[Byte]]): Try[(Seq[Seq[String]], Seq[Seq[String]], Seq[String], Seq[Seq[String]])] = Try {
    val triggerFuncBytes = Deser.parseArrays(bs.head)
    val triggerFunc = funcFromBytes(triggerFuncBytes)
    val descriptorFuncBytes = Deser.parseArrays(bs(1))
    val descriptorFunc = funcFromBytes(descriptorFuncBytes)
    val stateVar = paraFromBytes(bs(2))
    val stateMap = if (bs.length == 3) Seq()
                   else stateMapFromBytes(bs(3))
    (triggerFunc, descriptorFunc, stateVar, stateMap)
  }

  private def funcFromBytes(bs: Seq[Array[Byte]]): Seq[Seq[String]] = {
    bs.foldLeft(Seq.empty[Seq[String]]) { case (e, b) => {
      val (funcNameBytes, funcNameEnd) = Deser.parseArraySize(b, 0)
      val funcName = Deser.deserilizeString(funcNameBytes)
      val (listReturnNameBytes, listReturnNameEnd) = Deser.parseArraySize(b, funcNameEnd)
      val listReturnNames = paraFromBytes(listReturnNameBytes)
      val listParaNameBytes = b.slice(listReturnNameEnd, b.length)
      val listParaNames = paraFromBytes(listParaNameBytes)
      e :+ (listReturnNames ++ Seq(funcName) ++ listParaNames)
    }
    }
  }

  private def paraFromBytes(bytes: Array[Byte]): Seq[String] = {
    val listParaNameBytes = Deser.parseArrays(bytes)
    listParaNameBytes.foldLeft(Seq.empty[String]) { case (e, b) => {
      val paraName = Deser.deserilizeString(b)
      e :+ paraName
    }
    }
  }

  private def stateMapFromBytes(bytes: Array[Byte]): Seq[Seq[String]] = {
    val stateMapBytesList = Deser.parseArrays(bytes)
    stateMapBytesList.foldLeft(Seq.empty[Seq[String]]) {case (e, b) => {
      val stm: Seq[String] = Deser.parseArrays(b).map(x => Deser.deserilizeString(x))
      e :+ stm
    }}
  }

  private def checkTextual(textual: (Seq[Seq[String]], Seq[Seq[String]], Seq[String], Seq[Seq[String]])): Boolean = {
    val (trigger, descriptor, stateVariable, stateMap) = textual
    trigger.flatten.forall(x => identifierCheck(x)) && descriptor.flatten.forall(x => identifierCheck(x)) &&
      stateVariable.forall(x => identifierCheck(x)) && stateMap.flatten.forall(x => identifierCheck(x))
  }

  private def identifierCheck(str: String): Boolean = {
    def checkChar(c: Char): Boolean = {
      (c == '_') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    }

    val illegalIdf: List[String] = List("register", "unit", "while", "if", "for", "return", "match", "context",
      "contract", "int", "long", "short", "boolean", "trait", "lazy", "class", "else", "true", "false", "private",
      "val", "var", "try", "catch", "throw", "define", "transaction", "else", "public", "jump", "trigger", "then")
    if ((str.head == '_') || (str.head >= 'a' && str.head <= 'z') || (str.head >= 'A' && str.head <= 'Z')) {
      str.tail.forall(x => checkChar(x)) && !illegalIdf.contains(str.toLowerCase)
    } else {
      false
    }
  }
}
