package tools

import com.google.common.primitives.Shorts
import vsys.contract.ContractPermitted
import scorex.serialization.Deser
import vsys.contract.Contract.{LanguageCodeByteLength, LanguageVersionByteLength}

import vsys.state.opcdiffs.AssertOpcDiff.AssertType
import vsys.state.opcdiffs.CDBVOpcDiff.CDBVType
import vsys.state.opcdiffs.CDBVROpcDiff.CDBVRType
import vsys.state.opcdiffs.LoadOpcDiff.LoadType
import vsys.state.opcdiffs.OpcDiffer.OpcType
import vsys.state.opcdiffs.TDBAOpcDiff.TDBAType
import vsys.state.opcdiffs.TDBAROpcDiff.TDBARType
import vsys.state.opcdiffs.TDBOpcDiff.TDBType
import vsys.state.opcdiffs.TDBROpcDiff.TDBRType

import scala.util.{Failure, Success, Try}

object ContractTranslator extends App {
  val bytes = ContractPermitted.contract.bytes.arr
  val languageCode = bytes.slice(0, LanguageCodeByteLength)
  val languageVersion = bytes.slice(LanguageCodeByteLength, LanguageCodeByteLength + LanguageVersionByteLength)

  println("Language Code: " + convertBytesToHex(languageCode))
  println("Language Version: " + convertBytesToHex(languageVersion))

  val (initializer, initializerEnd) = Deser.parseArraySize(bytes, LanguageCodeByteLength + LanguageVersionByteLength)

  println("Initializer: " + convertBytesToHex(initializer))

  val (descriptorBytes, descriptorEnd) = Deser.parseArraySize(bytes, initializerEnd)
  val descriptor = Deser.parseArrays(descriptorBytes)

  println("Descriptor:")
  printSeqHex(descriptor)

  val (stateVarBytes, stateVarEnd) = Deser.parseArraySize(bytes, descriptorEnd)
  val stateVar = Deser.parseArrays(stateVarBytes)

  println("State Variable:")
  printSeqHex(stateVar)

  val texture = Deser.parseArrays(bytes.slice(stateVarEnd, bytes.length))
  val textureStr = textureFromBytes(texture)

  val dataTypeList = Seq("PublicKey", "Address", "Amount", "Int32", "ShortText", "ContractAccount", "Account")

  printTexture(textureStr)

  printSeqFun(Seq(initializer), 0)
  printSeqFun(descriptor, 1)

  def printFunction(bytes: Array[Byte], tp: Int): Unit = {
    opcFromBytes(bytes) match {
      case Success((funIdx, retType, listParaTypes, listOpcLines)) =>
        if (listOpcLines.forall(_.length < 2)) {
          println("Function print fail. Invalid OPC lines.")
        } else {
          val funString = if (tp == 0) textureStr.get._1 else textureStr.get._2
          val idx = if (tp == 0) funIdx.toInt else funIdx.toInt - 1
          print(funString(idx)(1) + " function " + funString(idx)(0) + "(")
          var tmp = funString(idx).tail.tail
          List.range(0, listParaTypes.size).foreach { i =>
            print(dataTypeList(listParaTypes(i) - 1) + " " + tmp(i))
            if (i < listParaTypes.size - 1) print(", ")
          }
          println(")")
          listOpcLines.foreach { case l =>
            print("    ")
            val x = opcToName(l, tmp)
            tmp = x._2
            println(x._1)
            println("    " + convertBytesToHex(l))
          }
          println()
        }
      case Failure(_) => println("Function print fail. Invalid function.")
    }
  }

  def printSeqFun(data: Seq[Array[Byte]], tp: Int): Unit = {
    List.range(0, data.size).foreach { i =>
      printFunction(data(i), tp)
    }
  }

  def convertBytesToHex(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x ", Byte.box(b)))
    }
    sb.toString
  }

  def printSeqHex(data: Seq[Array[Byte]]): Unit = {
    List.range(0, data.size).foreach { i =>
      println("%02d".format(i + 1) + " | " + convertBytesToHex(data(i)))
    }
  }

  def printSeqSeqString(s: Seq[Seq[String]]): Unit = {
    List.range(0, s.size).foreach { i =>
      println("%02d".format(i + 1) + " | " + s(i).mkString(", "))
    }
  }

  def printTexture(t: Try[(Seq[Seq[String]], Seq[Seq[String]], Seq[String])]): Unit = {
    if (t.isFailure) println("Invalid Texture")
    else {
      val r = t.get
      println("Initializer Functions:")
      printSeqSeqString(r._1)
      println("Descriptor Functions:")
      printSeqSeqString(r._2)
      println("State Variables:")
      List.range(0, r._3.size).foreach { i =>
        println("%02d".format(i + 1) + " | " + r._3(i))
      }
    }
  }

  private def textureFromBytes(bs: Seq[Array[Byte]]): Try[(Seq[Seq[String]], Seq[Seq[String]], Seq[String])] = Try {
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
      val (returnNameBytes, returnNameEnd) = Deser.parseArraySize(b, funcNameEnd)
      val returnName = Deser.deserilizeString(returnNameBytes)
      val listParaNameBytes = b.slice(returnNameEnd, b.length)
      val listParaNames = paraFromBytes(listParaNameBytes)
      e :+ (funcName +: returnName +: listParaNames)
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

  private def opcFromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Seq[Array[Byte]])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val (protoTypeBytes, protoTypeEnd) = Deser.parseArraySize(bytes, 2)
    val returnType = protoTypeBytes.head
    val listParaTypes = protoTypeBytes.tail
    val (listOpcLinesBytes, _) = Deser.parseArraySize(bytes, protoTypeEnd)
    val listOpcLines = Deser.parseArrays(listOpcLinesBytes)
    (funcIdx, returnType, listParaTypes, listOpcLines)
  }

  def opcToName(data: Array[Byte], nameList: Seq[String]): (String, Seq[String]) = {
    val x = data(0)
    val y = data(1)
    val stateNameList = textureStr.get._3
    x match {

      case opcType: Byte if opcType == OpcType.AssertOpc.id =>
        y match {
          case opcType: Byte if opcType == AssertType.GteqZeroAssert.id => ("opc_assert_gteq0", nameList)
          case opcType: Byte if opcType == AssertType.LteqAssert.id => ("opc_assert_lteq", nameList)
          case opcType: Byte if opcType == AssertType.LtInt64Assert.id => ("opc_assert_ltInt64", nameList)
          case opcType: Byte if opcType == AssertType.GtZeroAssert.id => ("opc_assert_gt0", nameList)
          case opcType: Byte if opcType == AssertType.EqAssert.id => ("opc_assert_eq", nameList)
          case opcType: Byte if opcType == AssertType.IsCallerOriginAssert.id => ("opc_assert_caller " + nameList(data(2)), nameList)
          case opcType: Byte if opcType == AssertType.IsSignerOriginAssert.id => ("opc_assert_singer " + nameList(data(2)), nameList)
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.LoadOpc.id =>
        y match {
          case opcType: Byte if opcType == LoadType.SignerLoad.id => ("opc_load_env_signer singer", nameList :+ "singer")
          case opcType: Byte if opcType == LoadType.CallerLoad.id => ("opc_load_env_caller caller", nameList :+ "caller")
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.CDBVOpc.id =>
        y match {
          case opcType: Byte if opcType == CDBVType.SetCDBV.id => ("opc_cdbv_set db." + stateNameList(data(2)) + " " + nameList(data(3)), nameList)
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.CDBVROpc.id =>
        y match {
          case opcType: Byte if opcType == CDBVRType.GetCDBVR.id => ("opc_cdbvr_get db." + stateNameList(data(2)) + " " + stateNameList(data(2)), nameList :+ stateNameList(data(2)))
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.TDBOpc.id =>
        y match {
          case opcType: Byte if opcType == TDBType.NewTokenTDB.id => ("opc_tdb_new " + nameList(data(6)) + " " + nameList(data(7)) + " " + nameList(data(8)), nameList)
          case opcType: Byte if opcType == TDBType.SplitTDB.id => ("opc_tdb_split " + nameList(data(3)) + " " + nameList(data(4)), nameList)
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.TDBROpc.id =>
        y match {
          case opcType: Byte if opcType == TDBRType.GetTDBR.id => ("opc_tdbr_get tdb." + stateNameList(data(2)) + " " + nameList(data(3)) + " "  + stateNameList(data(2)), nameList :+ stateNameList(data(2)))
          case opcType: Byte if opcType == TDBRType.TotalTDBR.id => ("opc_tdbr_total tdb." + stateNameList(data(2)) + " " + nameList(data(3)) + " "  + stateNameList(data(2)), nameList :+ stateNameList(data(2)))
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.TDBAOpc.id =>
        y match {
          case opcType: Byte if opcType == TDBAType.DepositTDBA.id => ("opc_tdba_deposit " + nameList(data(4)) + " " + nameList(data(5)) + " " + nameList(data(6)), nameList)
          case opcType: Byte if opcType == TDBAType.WithdrawTDBA.id => ("opc_tdba_withdraw " + nameList(data(3)) + " " + nameList(data(4)) + " " + nameList(data(5)), nameList)
          case opcType: Byte if opcType == TDBAType.TransferTDBA.id => ("opc_tdba_transfer " + nameList(data(2)) + " " + nameList(data(3)) + " " + nameList(data(4)) + " " + nameList(data(5)), nameList)
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.TDBAROpc.id =>
        y match {
          case opcType: Byte if opcType == TDBARType.BalanceTBDAR.id => ("opc_tdbar_balance " + nameList(data(2)) + " " + nameList(data(3)) + " balance", nameList :+ "balance")
          case _ => ("--- invalid opc code ---", nameList)
        }

      case opcType: Byte if opcType == OpcType.ReturnOpc.id => ("opc_return_last " + nameList.last, nameList)

      case _ => ("--- invalid opc code ---", nameList)

    }
  }


}
