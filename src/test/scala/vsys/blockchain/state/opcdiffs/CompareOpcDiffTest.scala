package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractUnsupportedOPC}

import scala.util.{Left, Right}

class CompareOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test compare opcs") {
    CompareOpcDiff.numBiComparation(DataEntry(Longs.toByteArray(1), DataType.Amount), DataEntry(
      Longs.toByteArray(1), DataType.Amount), CompareOpcDiff.ge) should be (
      Right(DataEntry(Array(1.toByte), DataType.Boolean)))
    CompareOpcDiff.numBiComparation(DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
      Longs.toByteArray(1), DataType.Amount), CompareOpcDiff.ge) should be (
      Right(DataEntry(Array(0.toByte), DataType.Boolean)))
    CompareOpcDiff.numBiComparation(DataEntry(Ints.toByteArray(0), DataType.Balance), DataEntry(
      Ints.toByteArray(1), DataType.Balance), CompareOpcDiff.ge) should be (
      Left(ContractUnsupportedOPC))
    CompareOpcDiff.numBiComparation(DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
      Ints.toByteArray(1), DataType.Int32), CompareOpcDiff.ge) should be (
      Left(ContractDataTypeMismatch))
  }
}
