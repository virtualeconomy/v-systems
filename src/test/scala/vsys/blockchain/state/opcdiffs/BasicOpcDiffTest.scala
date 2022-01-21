package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs, Shorts}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.DataType.MaxShortBytesLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractUnsupportedOPC, InvalidDataEntry}

import scala.util.{Left, Right}

class BasicOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test basic opcs") {
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      BasicOpcDiff.add) should be (Right(DataEntry(Longs.toByteArray(2), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.add) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      BasicOpcDiff.add) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.add) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      BasicOpcDiff.add) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.add) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      BasicOpcDiff.minus) should be (Right(DataEntry(Longs.toByteArray(0), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(2), DataType.Amount),
      BasicOpcDiff.minus) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(DataType.arrayShortLengthToByteArray(BigInt(1).toByteArray) ++ BigInt(1).toByteArray, DataType.BigInteger),
      DataEntry(DataType.arrayShortLengthToByteArray(BigInt(2).toByteArray) ++ BigInt(2).toByteArray, DataType.BigInteger),
      BasicOpcDiff.minus) should be (Right(DataEntry(DataType.arrayShortLengthToByteArray(BigInt(-1).toByteArray) ++ BigInt(-1).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.minus) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(-1), DataType.Amount),
      BasicOpcDiff.minus) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.minus) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      BasicOpcDiff.minus) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.minus) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      BasicOpcDiff.multiply) should be (Right(DataEntry(Longs.toByteArray(1), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.multiply) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount),
      DataEntry(Longs.toByteArray(2), DataType.Amount),
      BasicOpcDiff.multiply) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.multiply) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger),
      BasicOpcDiff.multiply) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(4).toByteArray.length.toShort) ++ BigInt(4).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray((BigInt(1) << 255*8).toByteArray.length.toShort) ++ (BigInt(1) << 255*8).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray((BigInt(1) << 255*8).toByteArray.length.toShort) ++ (BigInt(1) << 255*8).toByteArray, DataType.BigInteger),
      BasicOpcDiff.multiply) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.multiply) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      BasicOpcDiff.divide) should be (Right(DataEntry(Longs.toByteArray(1), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.divide) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      BasicOpcDiff.divide) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.divide) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger),
      BasicOpcDiff.divide) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      BasicOpcDiff.divide) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.divide) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      BasicOpcDiff.minimum) should be (Right(DataEntry(Longs.toByteArray(0), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.minimum) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.minimum) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      BasicOpcDiff.minimum) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.minimum) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount),
      BasicOpcDiff.maximum) should be (Right(DataEntry(Longs.toByteArray(1), DataType.Amount)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.maximum) should be (Left(ContractDataTypeMismatch))
    BasicOpcDiff.numBiOperation(
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      DataEntry(Longs.toByteArray(1), DataType.ShortText),
      BasicOpcDiff.maximum) should be (Left(ContractUnsupportedOPC))

    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger),
      DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger),
      BasicOpcDiff.maximum) should be (Right(DataEntry(
      Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.numBiOperation(
      DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger),
      DataEntry(Longs.toByteArray(1), DataType.Timestamp),
      BasicOpcDiff.maximum) should be (Left(ContractDataTypeMismatch))

    BasicOpcDiff.sqrt(DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Left(ContractUnsupportedOPC))
    BasicOpcDiff.sqrt(DataEntry(Shorts.toByteArray(BigInt(-1).toByteArray.length.toShort) ++ BigInt(-1).toByteArray, DataType.BigInteger)
      ) should be (Left(ValidationError.OverflowError))
    BasicOpcDiff.sqrt(DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger)
      ) should be (Right(DataEntry(Shorts.toByteArray(BigInt(1).toByteArray.length.toShort) ++ BigInt(1).toByteArray, DataType.BigInteger)))
    BasicOpcDiff.sqrt(DataEntry(Shorts.toByteArray(BigInt(4).toByteArray.length.toShort) ++ BigInt(4).toByteArray, DataType.BigInteger)
      ) should be (Right(DataEntry(Shorts.toByteArray(BigInt(2).toByteArray.length.toShort) ++ BigInt(2).toByteArray, DataType.BigInteger)))

    BasicOpcDiff.concat(
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (
      Right(DataEntry.create(Longs.toByteArray(1) ++ Longs.toByteArray(1),
        DataType.ShortBytes).right.get))
    BasicOpcDiff.concat(
      DataEntry(Array.fill(MaxShortBytesLength){0}, DataType.ShortBytes),
      DataEntry(Array.fill(1){0}, DataType.ShortBytes),
      ) should be (Left(InvalidDataEntry))

    BasicOpcDiff.constantGet(Array(DataType.Amount.id.toByte) ++ Longs.toByteArray(1),
      ) should be (Right(DataEntry(Longs.toByteArray(1), DataType.Amount)))
    BasicOpcDiff.constantGet(Array(DataType.Amount.id.toByte) ++ Ints.toByteArray(1),
      ) should be (Left(InvalidDataEntry))
  }
}
