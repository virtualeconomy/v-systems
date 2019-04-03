package vsys.contract

import org.scalacheck.Gen


trait ContractGen extends OpcFunction with StateVar with Textual {
  val lowerBoundOfNumber = 1
  val upperBoundOfNumber = 0x7f

  def languageCodeFromLengthGen(length: Int): Gen[Array[Byte]] = for {
    languageCodeByte <- Gen.listOfN(length, Gen.choose(lowerBoundOfNumber, upperBoundOfNumber).map(_.toByte)).map(_.toArray)
  } yield languageCodeByte

  def languageVersionFromLengthGen(length: Int): Gen[Array[Byte]] = for {
    languageVersion <- Gen.listOfN(length, Gen.choose(lowerBoundOfNumber, upperBoundOfNumber).map(_.toByte)).map(_.toArray)
  } yield languageVersion

  def contractNewGen(lang: Seq[Int], init: Gen[Array[Byte]], desc: Gen[Seq[Array[Byte]]], state: Gen[Seq[Array[Byte]]], text: Gen[Seq[Array[Byte]]]): Gen[Contract] = for {
    languageCode <- languageCodeFromLengthGen(lang.head)
    languageVersion <- languageVersionFromLengthGen(lang.tail.head)
    initializer <- init
    descriptor <- desc
    stateVar <- state
    textual <- text
  } yield Contract.buildContract(languageCode, languageVersion, initializer, descriptor, stateVar, textual).right.get

  def contractRandomGen(): Gen[Contract] = for {
    contract <- contractNewGen(Seq(4, 4), aFunctionRandomGen(), descriptorRandomGen(), stateVarRandomGen(), textualRandomGen())
  } yield contract
}
