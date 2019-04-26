package tools

import com.google.common.primitives.Shorts
import vsys.contract.ContractPermitted
import scorex.serialization.Deser
import vsys.contract.Contract.{LanguageCodeByteLength, LanguageVersionByteLength}
import scorex.crypto.encode.Base58

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

  println(Base58.encode(bytes))
  print("Contract Bytes Length:")
  println(bytes.length)

  val languageCode = bytes.slice(0, LanguageCodeByteLength)
  val languageVersion = bytes.slice(LanguageCodeByteLength, LanguageCodeByteLength + LanguageVersionByteLength)

  println("Language Code: " + convertBytesToHex(languageCode))
  println("Language Version: " + convertBytesToHex(languageVersion))

  val (triggerBytes, triggerEnd) = Deser.parseArraySize(bytes, LanguageCodeByteLength + LanguageVersionByteLength)
  val trigger = Deser.parseArrays(triggerBytes)

  println("Triggers:")
  printSeqHex(trigger)

  val (descriptorBytes, descriptorEnd) = Deser.parseArraySize(bytes, triggerEnd)
  val descriptor = Deser.parseArrays(descriptorBytes)

  println("Descriptor:")
  printSeqHex(descriptor)

  val (stateVarBytes, stateVarEnd) = Deser.parseArraySize(bytes, descriptorEnd)
  val stateVar = Deser.parseArrays(stateVarBytes)

  println("State Variables:")
  printSeqHex(stateVar)

  val texture = Deser.parseArrays(bytes.slice(stateVarEnd, bytes.length))
  val textualStr = textualFromBytes(texture)

  val dataTypeList = Seq("PublicKey", "Address", "Amount", "Int32", "ShortText", "ContractAccount", "Account")

  printTexture(textualStr)

  println("Triggers:")
  printSeqFun(trigger, 0)
  println("Descriptor Functions:")
  printSeqFun(descriptor, 1)

  def printFunction(bytes: Array[Byte], tp: Int): Unit = {
    opcFromBytes(bytes) match {
      case Success((funcIdx, funcType, listReturnTypes, listParaTypes, listOpcLines)) =>
        if (listOpcLines.forall(_.length < 2)) {
          println("Function print fail. Invalid OPC lines.")
        } else {
          val ftString = if (tp == 0) textualStr.get._1 else textualStr.get._2
          val idx = funcIdx.toInt
          // TODO
          // need a more complex ftTypes check
          // print function or trigger type
          val ftType = if (tp == 0) "onInit trigger" else "public function"
          print(ftType + " ")

          // print function or trigger name
          val retNum = listReturnTypes.size
          val ftName = ftString(idx)(retNum)
          print(ftName + "(")

          // print input variables
          val inputVarList = ftString(idx).slice(retNum + 1, ftString(idx).length)
          List.range(0, listParaTypes.size).foreach { i =>
            print(dataTypeList(listParaTypes(i) - 1) + " " + inputVarList(i))
            if (i < listParaTypes.size - 1) print(", ")
          }
          print(")")

          //print return variables
          if (listReturnTypes.length == 1) {
            print(" return ")
            println(dataTypeList(listReturnTypes(0) - 1) + " " + ftString(idx)(0))
          } else if (listReturnTypes.length > 1){
            print(" return (")
            List.range(0, retNum).foreach { i =>
              print(dataTypeList(listReturnTypes(i) - 1) + " " + ftString(idx)(i))
              if (i < listParaTypes.size - 1) print(", ")
            }
            println(")")
          } else println()

          // print function or trigger head opc
          print("| ")
          print(convertBytesToHex(Shorts.toByteArray(funcIdx)))
          print("| ")
          print(convertBytesToHex(Array(funcType)))
          print("| ")
          print(convertBytesToHex(listParaTypes))
          print("| ")
          print(convertBytesToHex(listReturnTypes))
          println("|")

          // print opc lines
          listOpcLines.foreach { case l =>
            print("    ")
            val x = opcToName(l, inputVarList)
            println(x)
            print("    | " + convertBytesToHex(l.slice(0, 2)))
            print("| ")
            println(convertBytesToHex(l.slice(2, l.length)) + "|")
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
      println("%02d".format(i) + " | " + convertBytesToHex(data(i)))
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
      println("Trigger Functions:")
      printSeqSeqString(r._1)
      println("Descriptor Functions:")
      printSeqSeqString(r._2)
      println("State Variables:")
      List.range(0, r._3.size).foreach { i =>
        println("%02d".format(i) + " | " + r._3(i))
      }
    }
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

  private def opcFromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Array[Byte], Seq[Array[Byte]])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val funcType = bytes(2)
    val (listReturnTypes, listReturnTypeEnd) = Deser.parseArraySize(bytes, 3)
    val (listParaTypes, listParaTypeEnd) = Deser.parseArraySize(bytes, listReturnTypeEnd)
    val (listOpcLinesBytes, _) = Deser.parseArraySize(bytes, listParaTypeEnd)
    val listOpcLines = Deser.parseArrays(listOpcLinesBytes)
    (funcIdx, funcType,listReturnTypes, listParaTypes, listOpcLines)
  }

  def opcToName(data: Array[Byte], nameList: Seq[String]): String = {
    val x = data(0)
    val y = data(1)
    val stateNameList = textualStr.get._3
    x match {

      case opcType: Byte if opcType == OpcType.AssertOpc.id =>
        y match {
          case opcType: Byte if opcType == AssertType.GteqZeroAssert.id => "opc_assert_gteq0"
          case opcType: Byte if opcType == AssertType.LteqAssert.id => "opc_assert_lteq"
          case opcType: Byte if opcType == AssertType.LtInt64Assert.id => "opc_assert_ltInt64"
          case opcType: Byte if opcType == AssertType.GtZeroAssert.id => "opc_assert_gt0"
          case opcType: Byte if opcType == AssertType.EqAssert.id => "opc_assert_eq"
          case opcType: Byte if opcType == AssertType.IsCallerOriginAssert.id => "operation.check.assertCaller(" + nameList(data(2)) + ")"
          case opcType: Byte if opcType == AssertType.IsSignerOriginAssert.id => "operation.check.assertSigner(" + nameList(data(2)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.LoadOpc.id =>
        y match {
          case opcType: Byte if opcType == LoadType.SignerLoad.id => nameList(data(2)) + " = operation.env.getSigner()"
          case opcType: Byte if opcType == LoadType.CallerLoad.id => nameList(data(2)) + " = operation.env.getCaller()"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.CDBVOpc.id =>
        y match {
          case opcType: Byte if opcType == CDBVType.SetCDBV.id => "operation.db.setVariable(" + "db." + stateNameList(data(2)) + ", " + nameList(data(3)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.CDBVROpc.id =>
        y match {
          case opcType: Byte if opcType == CDBVRType.GetCDBVR.id => nameList(data(3)) + " = operation.db.getVariable(db." + stateNameList(data(2)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.TDBOpc.id =>
        y match {
          case opcType: Byte if opcType == TDBType.NewTokenTDB.id => "operation.token.new(" + nameList(data(2)) + ", " + nameList(data(3)) + ", " + nameList(data(4)) + ")"
          case opcType: Byte if opcType == TDBType.SplitTDB.id => "operation.token.split(" + nameList(data(2)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.TDBROpc.id =>
        y match {
          case opcType: Byte if opcType == TDBRType.MaxTDBR.id => nameList(data(2)) + " = operation.token.getMaxSupply(" + ")"
          case opcType: Byte if opcType == TDBRType.TotalTDBR.id => nameList(data(2)) + " = operation.token.getTotalSupply(" + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.TDBAOpc.id =>
        y match {
          case opcType: Byte if opcType == TDBAType.DepositTDBA.id => "operation.token.deposit(" + nameList(data(2)) + ", " + nameList(data(3)) + ")"
          case opcType: Byte if opcType == TDBAType.WithdrawTDBA.id => "operation.token.withdraw(" + nameList(data(2)) + ", " + nameList(data(3)) + ")"
          case opcType: Byte if opcType == TDBAType.TransferTDBA.id => "operation.token.transfer(" + nameList(data(2)) + ", " + nameList(data(3)) + ", " + nameList(data(4)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.TDBAROpc.id =>
        y match {
          case opcType: Byte if opcType == TDBARType.BalanceTBDAR.id => nameList(data(3)) + " = operation.token.getBalance(" + nameList(data(2)) + ")"
          case _ => "--- invalid opc code ---"
        }

      case opcType: Byte if opcType == OpcType.ReturnOpc.id => "operation.control.return(" + nameList(data(2)) + ")"

      case _ => "--- invalid opc code ---"

    }
  }


}
