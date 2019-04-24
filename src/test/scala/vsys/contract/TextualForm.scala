package vsys.contract

import org.scalacheck.{Arbitrary, Gen}
import scorex.serialization.Deser

trait TextualForm {
  private val fixedSize: Short = 4

  def textureRandomGen(): Gen[Seq[Array[Byte]]] = for {
    textual <- Gen.listOfN(fixedSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Seq(textual)

  def textureGen(initialization: Gen[Array[Byte]], description: Gen[Array[Byte]], stateVar: Gen[Array[Byte]]): Gen[Seq[Array[Byte]]] = for {
    init <- initialization
    desc <-  description
    state <- stateVar
  } yield Seq(init, desc, state)

  val stateVarTexture: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(TextualStateVar.stateVarName.map(x => Deser.serilizeString(x))))
  val initializerTexture: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(Seq(TextureFun.initFuncBytes.sample.get)))
  val descriptorTexture: Gen[Array[Byte]] = Gen.const(Deser.serializeArrays(Seq(TextureFun.supersedeFuncBytes.sample.get,
    TextureFun.issueFuncBytes.sample.get, TextureFun.destroyFuncBytes.sample.get, TextureFun.splitFuncBytes.sample.get,
    TextureFun.sendFuncBytes.sample.get, TextureFun.transferFuncBytes.sample.get, TextureFun.depositFuncBytes.sample.get,
    TextureFun.withdrawFuncBytes.sample.get, TextureFun.totalSupplyFuncBytes.sample.get, TextureFun.maxSupplyFuncBytes.sample.get,
    TextureFun.balanceOfFuncBytes.sample.get, TextureFun.getIssuerFuncBytes.sample.get)))

  val textureRightGen: Gen[Seq[Array[Byte]]] = textureGen(initializerTexture, descriptorTexture, stateVarTexture)

}

object TextureFun {
  def textureFunGen(name: String, ret: Seq[String], para: Seq[String]): Gen[Array[Byte]] =  for {
    funcByte <- Gen.const(Deser.serializeArray(Deser.serilizeString(name)))
    returnByte <- Gen.const(Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x)))))
    paraByte <- Gen.const(Deser.serializeArrays(para.map(x => Deser.serilizeString(x))))
    texture <- Gen.const(funcByte.array ++ returnByte.array ++ paraByte.array)
  } yield texture

  val initPara: Seq[String] = Seq("max", "unity", "tokenDescription", "signer")
  val supersedePara: Seq[String] = Seq("newIssuer", "maker")
  val issuePara: Seq[String] = Seq("amount", "tokenIndex", "issuer")
  val destroyPara: Seq[String] = Seq("amount", "tokenIndex", "issuer")
  val splitPara: Seq[String] = Seq("newUnity", "tokenIndex", "issuer")
  val sendPara: Seq[String] = Seq("receipt", "amount", "tokenIndex", "caller")
  val transferPara: Seq[String]= Seq("sender", "receipt", "amount", "tokenIndex")
  val depositPara: Seq[String] = Seq("sender", "smart", "amount", "tokenIndex")
  val withdrawPara: Seq[String]= Seq("smart", "receipt", "amount", "tokenIndex")
  val totalSupplyPara: Seq[String] = Seq("tokenIndex", "total")
  val maxSupplyPara: Seq[String] = Seq("tokenIndex", "max")
  val balanceOfPara: Seq[String] = Seq("address", "tokenIndex", "balance")
  val getIssuerPara: Seq[String] = Seq("issuer")

  val initFuncBytes: Gen[Array[Byte]] = textureFunGen("init", Seq(), initPara)
  val supersedeFuncBytes: Gen[Array[Byte]] = textureFunGen("supersede", Seq(), supersedePara)
  val issueFuncBytes: Gen[Array[Byte]] = textureFunGen("issue", Seq(), issuePara)
  val destroyFuncBytes: Gen[Array[Byte]] = textureFunGen("destroy", Seq(), destroyPara)
  val splitFuncBytes: Gen[Array[Byte]] = textureFunGen("split", Seq(), splitPara)
  val sendFuncBytes: Gen[Array[Byte]] = textureFunGen("send", Seq(), sendPara)
  val transferFuncBytes: Gen[Array[Byte]] = textureFunGen("transfer", Seq(), transferPara)
  val depositFuncBytes: Gen[Array[Byte]] = textureFunGen("deposit", Seq(), depositPara)
  val withdrawFuncBytes: Gen[Array[Byte]] = textureFunGen("withdraw", Seq(), withdrawPara)
  val totalSupplyFuncBytes: Gen[Array[Byte]] = textureFunGen("totalSupply", Seq("total"), totalSupplyPara)
  val maxSupplyFuncBytes: Gen[Array[Byte]] = textureFunGen("maxSupply", Seq("max"), maxSupplyPara)
  val balanceOfFuncBytes: Gen[Array[Byte]] = textureFunGen("balanceOf", Seq("balance"), balanceOfPara)
  val getIssuerFuncBytes: Gen[Array[Byte]] = textureFunGen("getIssuer", Seq("issuer"), getIssuerPara)
}

object TextualStateVar {
  val stateVarName = List("issuer", "maker")
}
