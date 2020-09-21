package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.DataType.MaxShortBytesLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractLocalVariableIndexOutOfRange,
  ContractUnsupportedOPC, InvalidDataEntry}

import scala.util.{Left, Right}

class BasicOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test basic opcs") {
    BasicOpcDiff.add(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(2), DataType.Amount))))
    BasicOpcDiff.add(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.add(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.add(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.add(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.minus(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    BasicOpcDiff.minus(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.minus(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.minus(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(-1), DataType.Amount),
      Seq.empty, 0) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.minus(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.multiply(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.multiply(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.multiply(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.multiply(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(2), DataType.Amount),
      Seq.empty, 0) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.multiply(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.divide(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.divide(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.divide(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.divide(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.divide(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.minimum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    BasicOpcDiff.minimum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.minimum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.minimum(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.maximum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.maximum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.maximum(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.maximum(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.concat(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0) should be (
      Right(Seq(DataEntry.create(Longs.toByteArray(1) ++ Longs.toByteArray(1),
        DataType.ShortBytes).right.get)))
    BasicOpcDiff.concat(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.concat(
      DataEntry(Array.fill(MaxShortBytesLength){0}, DataType.ShortBytes),
      DataEntry(Array.fill(1){0}, DataType.ShortBytes),
      Seq.empty, 0) should be (Left(InvalidDataEntry))

    BasicOpcDiff.constantGet(Array(DataType.Amount.id.toByte) ++ Longs.toByteArray(1),
      Seq.empty, 0) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.constantGet(Array(DataType.Amount.id.toByte) ++ Ints.toByteArray(1),
      Seq.empty, 0) should be (Left(InvalidDataEntry))
    BasicOpcDiff.constantGet(Array(DataType.Amount.id.toByte) ++ Longs.toByteArray(1),
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
  }
}
