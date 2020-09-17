package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractLocalVariableIndexOutOfRange,
  ContractUnsupportedOPC, InvalidDataEntry}

import scala.util.{Left, Right}

class BasicOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test basic opcs") {
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.add) should be (Right(Seq(DataEntry(Longs.toByteArray(2), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.add) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.add) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.add) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.add) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.minus) should be (Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.minus) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.minus) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(-1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.minus) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.minus) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.multiply) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.multiply) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.multiply) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(2), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.multiply) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.multiply) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.divide) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.divide) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.divide) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.divide) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.divide) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.minimum) should be (Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.minimum) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.minimum) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.minimum) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 0, BasicOpcDiff.maximum) should be (Right(Seq(DataEntry(Longs.toByteArray(1), DataType.Amount))))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      Seq.empty, 1, BasicOpcDiff.maximum) should be (Left(ContractLocalVariableIndexOutOfRange))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      Seq.empty, 0, BasicOpcDiff.maximum) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.operation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      Seq.empty, 0, BasicOpcDiff.maximum) should be (Left(ContractUnsupportedOPC))

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
      DataEntry(Array.fill(DataType.ShortBytes.maxLen){0}, DataType.ShortBytes),
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
