package vsys.contract

import org.scalacheck.{Arbitrary, Gen}

trait StateVar {
  private val fixedSize: Short = 2

  def stateVarRandomGen(): Gen[Seq[Array[Byte]]] = for {
    stateVar <- Gen.listOfN(fixedSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Seq(stateVar)

  def stateVarGen(stateVars: Seq[Array[Byte]]): Gen[Seq[Array[Byte]]] = for {
    stateVar <- Gen.const(stateVars)
  } yield stateVar

  def stateVarInitGen(state: Seq[Array[Byte]]): Gen[Seq[Array[Byte]]] = for {
    stateVar <- stateVarGen(state)
  } yield stateVar

  val stateVarInitRegisterContract: Gen[Seq[Array[Byte]]] = stateVarInitGen(Seq(Array(0.toByte, DataType.Address.id.toByte),
    Array(1.toByte, DataType.Address.id.toByte), Array(2.toByte, DataType.Amount.id.toByte),
    Array(3.toByte, DataType.Amount.id.toByte), Array(4.toByte, DataType.Amount.id.toByte),
    Array(5.toByte, DataType.ShortText.id.toByte)))
}