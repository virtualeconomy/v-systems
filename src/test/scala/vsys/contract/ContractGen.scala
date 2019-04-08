package vsys.contract

import org.scalacheck.Gen
import com.google.common.primitives.Ints



trait ContractGen extends OpcFunction with StateVar with Texture {

  def languageCodeFromLengthGen(code: Int): Gen[Array[Byte]] = for {
    languageCode <- Gen.const(Ints.toByteArray(code))
  } yield languageCode

  def languageVersionFromLengthGen(version: Int): Gen[Array[Byte]] = for {
    languageVersion <- Gen.const(Ints.toByteArray(version))
  } yield languageVersion

  def contractNewGen(languageCode: Int, languageVersion: Int, init: Gen[Array[Byte]], desc: Gen[Seq[Array[Byte]]], state: Gen[Seq[Array[Byte]]], text: Gen[Seq[Array[Byte]]]): Gen[Contract] = for {
    langCode <- languageCodeFromLengthGen(languageCode)
    langVer <- languageVersionFromLengthGen(languageVersion)
    initializer <- init
    descriptor <- desc
    stateVar <- state
    textual <- text
  } yield Contract.buildContract(langCode, langVer, initializer, descriptor, stateVar, textual).right.get

  def contractRandomGen(): Gen[Contract] = for {
    contract <- contractNewGen(4, 4, aFunctionRandomGen(), descriptorRandomGen(), stateVarRandomGen(), textureRandomGen())
  } yield contract
}
