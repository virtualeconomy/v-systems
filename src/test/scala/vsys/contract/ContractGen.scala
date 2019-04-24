package vsys.contract

import org.scalacheck.Gen
import com.google.common.primitives.Ints
import scorex.serialization.Deser

trait ContractGen extends OpcFunction with StateVar with TextualForm {

  def languageCodeFromLengthGen(code: String): Gen[Array[Byte]] = for {
    languageCode <- Gen.const(Deser.serilizeString(code))
  } yield languageCode

  def languageVersionFromLengthGen(version: Int): Gen[Array[Byte]] = for {
    languageVersion <- Gen.const(Ints.toByteArray(version))
  } yield languageVersion

  def contractNewGen(languageCode: String, languageVersion: Int, init: Gen[Seq[Array[Byte]]],
                     desc: Gen[Seq[Array[Byte]]], state: Gen[Seq[Array[Byte]]], text: Gen[Seq[Array[Byte]]]): Gen[Contract] = for {
    langCode <- languageCodeFromLengthGen(languageCode)
    langVer <- languageVersionFromLengthGen(languageVersion)
    initializer <- init
    descriptor <- desc
    stateVar <- state
    textual <- text
  } yield Contract.buildContract(langCode, langVer, initializer, descriptor, stateVar, textual).right.get

  def contractRandomGen(): Gen[Contract] = for {
    contract <- contractNewGen("vdds", 1, descriptorRandomGen(), descriptorRandomGen(), stateVarRandomGen(), textureRandomGen())
  } yield contract
}
