package vsys.blockchain.contract

import org.scalacheck.{Arbitrary, Gen}

trait StateVar {
  import StateVar._
  private val fixedSize: Short = 2

  def stateVarRandomGen(): Gen[Seq[Array[Byte]]] = for {
    stateVar <- Gen.listOfN(fixedSize, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield Seq(stateVar)

  def stateVarGen(stateVars: Seq[Array[Byte]]): Gen[Seq[Array[Byte]]] = for {
    stateVar <- Gen.const(stateVars)
  } yield stateVar

  val stateVarRightGen: Gen[Seq[Array[Byte]]] = stateVarGen(Seq(Array(issuer, DataType.Address.id.toByte),
    Array(maker, DataType.Address.id.toByte)))
}

object StateVar {
  val issuer: Byte = 0
  val maker: Byte = 1
}