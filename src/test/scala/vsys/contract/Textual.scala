package vsys.contract

import org.scalacheck.{Arbitrary, Gen}

trait Textual {
  private val fixedSize: Short = 4

  def textualRandomGen(): Gen[Seq[Array[Byte]]] = for {
    textual <- Gen.listOfN(fixedSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Seq(textual)

  def textualGen(length: Short, textuals: Gen[Seq[Array[Byte]]]): Gen[Seq[Array[Byte]]] = for {
    textual <- textuals
  } yield textual
}