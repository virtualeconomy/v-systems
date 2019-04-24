package vsys.contract

import com.google.common.primitives.Ints
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidContract
import scorex.utils.ScorexLogging

import scala.util.{Success, Try}

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + Base58.encode(languageCode) + ":" + Base58.encode(languageVersion)
  lazy val bytes: ByteStr = ByteStr(languageCode ++ languageVersion
    ++ Deser.serializeArray(Deser.serializeArrays(trigger))
    ++ Deser.serializeArray(Deser.serializeArrays(descriptor))
    ++ Deser.serializeArray(Deser.serializeArrays(stateVar))
    ++ Deser.serializeArrays(textual))

  val trigger: Seq[Array[Byte]]
  val descriptor: Seq[Array[Byte]]
  val stateVar: Seq[Array[Byte]]
  val textual: Seq[Array[Byte]]
  val languageCode: Array[Byte]
  val languageVersion: Array[Byte]

  lazy val json: JsObject = Json.obj(
    "languageCode" -> Deser.deserilizeString(languageCode),
    "languageVersion" -> Ints.fromByteArray(languageVersion),
    "trigger" -> trigger.map(p => Base58.encode(p)),
    "descriptor" -> descriptor.map(p => Base58.encode(p)),
    "stateVar" -> stateVar.map(p => Base58.encode(p)),
    "textual" -> textual.map(p => Base58.encode(p))
  )
}

object Contract extends ScorexLogging {

  val Prefix: String = "contract:"

  val MinContractByteSize = 8
  val MinContractStringSize: Int = base58Length(MinContractByteSize)
  val LanguageCodeByteLength = 4
  val LanguageVersionByteLength = 4
  val LanguageCodeByte: Array[Byte] = Deser.serilizeString("vdds")
  val LanguageVersionByte: Array[Byte] = Ints.toByteArray(1)

  def buildContract(languageCode: Array[Byte], languageVersion: Array[Byte],
                    trigger: Seq[Array[Byte]], descriptor: Seq[Array[Byte]],
                    stateVar: Seq[Array[Byte]], textual: Seq[Array[Byte]]): Either[ValidationError, Contract] = {
    case class ContractImpl(languageCode: Array[Byte], languageVersion: Array[Byte],
                            trigger: Seq[Array[Byte]], descriptor: Seq[Array[Byte]],
                            stateVar: Seq[Array[Byte]], textual: Seq[Array[Byte]]) extends Contract
    Right(ContractImpl(languageCode, languageVersion, trigger, descriptor, stateVar, textual))
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
      val textual = Deser.parseArrays(bytes.slice(stateVarEnd, bytes.length))
      if (isByteArrayValid(bytes, textual)){
        buildContract(languageCode, languageVersion, trigger, descriptor, stateVar, textual)
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

  def checkStateVar(stateVar: Array[Byte], dataType: DataType.Value): Boolean =
    stateVar.length == 2 && dataType == DataType(stateVar(1))

  private def isByteArrayValid(bytes: Array[Byte], textual: Seq[Array[Byte]]): Boolean = {
    val textualStr = textualFromBytes(textual)
    if (!(bytes sameElements ContractPermitted.contract.bytes.arr) &&
      !(bytes sameElements ContractPermitted.contractWithoutSplit.bytes.arr)) {
      log.warn(s"Illegal contract ${bytes.mkString(" ")}")
      false
    } else if (textualStr.isFailure ||
      !checkTexture(textualStr.getOrElse((Seq.empty[Seq[String]], Seq.empty[Seq[String]], Seq.empty[String])))) {
      log.warn(s"Illegal textual ${textual.mkString(" ")}")
      false
    } else true
  }

  private def textualFromBytes(bs: Seq[Array[Byte]]): Try[(Seq[Seq[String]], Seq[Seq[String]], Seq[String])] = Try {
    val initializerFuncBytes = Deser.parseArrays(bs.head)
    val initializerFunc = funcFromBytes(initializerFuncBytes)
    val descriptorFuncBytes = Deser.parseArrays(bs(1))
    val descriptorFunc = funcFromBytes(descriptorFuncBytes)
    val stateVar = paraFromBytes(bs.last)
    (initializerFunc, descriptorFunc, stateVar)
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

  private def checkTexture(textual: (Seq[Seq[String]], Seq[Seq[String]], Seq[String])): Boolean = {
    textual._1.flatten.forall(x => identifierCheck(x)) && textual._2.flatten.forall(x => identifierCheck(x)) &&
      textual._3.forall(x => identifierCheck(x))
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
