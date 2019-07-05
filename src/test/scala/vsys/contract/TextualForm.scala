package vsys.blockchain.contract

import org.scalacheck.{Arbitrary, Gen}
import vsys.utils.serialization.Deser

trait TextualForm {
  private val fixedSize: Short = 4

  def textualRandomGen(): Gen[Seq[Array[Byte]]] = for {
    textual <- Gen.listOfN(fixedSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Seq(textual)

  def textualGen(initialization: Gen[Array[Byte]], description: Gen[Array[Byte]], stateVar: Gen[Array[Byte]]): Gen[Seq[Array[Byte]]] = for {
    init <- initialization
    desc <-  description
    state <- stateVar
  } yield Seq(init, desc, state)

  val stateVarTextual: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(TextualStateVar.stateVarName.map(x => Deser.serilizeString(x))))
  val triggerTextual: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(Seq(TextualFun.initFuncBytes.sample.get)))
  val descriptorTextual: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(Seq(TextualFun.supersedeFuncBytes.sample.get,
    TextualFun.issueFuncBytes.sample.get, TextualFun.destroyFuncBytes.sample.get, TextualFun.splitFuncBytes.sample.get,
    TextualFun.sendFuncBytes.sample.get, TextualFun.transferFuncBytes.sample.get, TextualFun.depositFuncBytes.sample.get,
    TextualFun.withdrawFuncBytes.sample.get, TextualFun.totalSupplyFuncBytes.sample.get, TextualFun.maxSupplyFuncBytes.sample.get,
    TextualFun.balanceOfFuncBytes.sample.get, TextualFun.getIssuerFuncBytes.sample.get)))

  val textualRightGen: Gen[Seq[Array[Byte]]] = textualGen(triggerTextual, descriptorTextual, stateVarTextual)

}

object TextualFun {
  def textualFunGen(name: String, ret: Seq[String], para: Seq[String]): Gen[Array[Byte]] =  for {
    funcByte <- Gen.const(Deser.serializeArray(Deser.serilizeString(name)))
    returnByte <- Gen.const(Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x)))))
    paraByte <- Gen.const(Deser.serializeArrays(para.map(x => Deser.serilizeString(x))))
    texture <- Gen.const(funcByte.array ++ returnByte.array ++ paraByte.array)
  } yield texture

  val initPara: Seq[String] = Seq("max", "unity", "tokenDescription", "signer")
  val supersedePara: Seq[String] = Seq("newIssuer", "maker")
  val issuePara: Seq[String] = Seq("amount", "issuer")
  val destroyPara: Seq[String] = Seq("amount", "issuer")
  val splitPara: Seq[String] = Seq("newUnity", "issuer")
  val sendPara: Seq[String] = Seq("recipient", "amount", "caller")
  val transferPara: Seq[String]= Seq("sender", "recipient", "amount")
  val depositPara: Seq[String] = Seq("sender", "smart", "amount")
  val withdrawPara: Seq[String]= Seq("smart", "recipient", "amount")
  val totalSupplyPara: Seq[String] = Seq("total")
  val maxSupplyPara: Seq[String] = Seq("max")
  val balanceOfPara: Seq[String] = Seq("address", "balance")
  val getIssuerPara: Seq[String] = Seq("issuer")

  val initFuncBytes: Gen[Array[Byte]] = textualFunGen("init", Seq(), initPara)
  val supersedeFuncBytes: Gen[Array[Byte]] = textualFunGen("supersede", Seq(), supersedePara)
  val issueFuncBytes: Gen[Array[Byte]] = textualFunGen("issue", Seq(), issuePara)
  val destroyFuncBytes: Gen[Array[Byte]] = textualFunGen("destroy", Seq(), destroyPara)
  val splitFuncBytes: Gen[Array[Byte]] = textualFunGen("split", Seq(), splitPara)
  val sendFuncBytes: Gen[Array[Byte]] = textualFunGen("send", Seq(), sendPara)
  val transferFuncBytes: Gen[Array[Byte]] = textualFunGen("transfer", Seq(), transferPara)
  val depositFuncBytes: Gen[Array[Byte]] = textualFunGen("deposit", Seq(), depositPara)
  val withdrawFuncBytes: Gen[Array[Byte]] = textualFunGen("withdraw", Seq(), withdrawPara)
  val totalSupplyFuncBytes: Gen[Array[Byte]] = textualFunGen("totalSupply", Seq("total"), totalSupplyPara)
  val maxSupplyFuncBytes: Gen[Array[Byte]] = textualFunGen("maxSupply", Seq("max"), maxSupplyPara)
  val balanceOfFuncBytes: Gen[Array[Byte]] = textualFunGen("balanceOf", Seq("balance"), balanceOfPara)
  val getIssuerFuncBytes: Gen[Array[Byte]] = textualFunGen("getIssuer", Seq("issuer"), getIssuerPara)
}

object TextualStateVar {
  val stateVarName = List("issuer", "maker")
}
