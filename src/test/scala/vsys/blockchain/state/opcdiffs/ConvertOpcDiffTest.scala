package vsys.blockchain.state.opcdiffs

import org.scalatest.{Matchers, PropSpec}
import com.google.common.primitives.{Ints, Longs, Shorts}
import vsys.blockchain.contract.{DataEntry, DataType}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.blockchain.transaction.ValidationError

class ConvertOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {
    val AmountDataTypeObj = DataEntry(Array(3), DataType.DataTypeObj)
    val Int32DataTypeObj = DataEntry(Array(4), DataType.DataTypeObj)
    val TimestampDataTypeObj = DataEntry(Array(9), DataType.DataTypeObj)
    val BigIntDataTypeObj = DataEntry(Array(14), DataType.DataTypeObj)

    property("test convertions of all numerical types to Amount") {
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(1000), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(1000), DataType.Int32), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(1000), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Timestamp), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(1000), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(0), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(0), DataType.Int32), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(0), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(0), DataType.Timestamp), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(0), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(Long.MaxValue).toByteArray.length.toShort) ++ BigInt(Long.MaxValue).toByteArray, DataType.BigInteger), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(Int.MaxValue), DataType.Int32), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Amount))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Timestamp), AmountDataTypeObj) should be (
        Right(DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Amount))
      )
    }
    property("test convertions of all numerical types to Int32") {
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(1000), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Amount), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(1000), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Timestamp), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(1000), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(0), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(0), DataType.Amount), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(0), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(0), DataType.Timestamp), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(0), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(Int.MaxValue).toByteArray.length.toShort) ++ BigInt(Int.MaxValue).toByteArray, DataType.BigInteger), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(Int.MaxValue), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Amount), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(Int.MaxValue), DataType.Int32))
      )
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Timestamp), Int32DataTypeObj) should be (
        Right(DataEntry(Ints.toByteArray(Int.MaxValue), DataType.Int32))
      )
    }
   property("test convertion of all numerical types to Timestamp") {
     BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(1000), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(1000), DataType.Int32), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(1000), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Amount), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(1000), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(0).toByteArray.length.toShort) ++ BigInt(0).toByteArray, DataType.BigInteger), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(0), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(0), DataType.Int32), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(0), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(0), DataType.Amount), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(0), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray(BigInt(Long.MaxValue).toByteArray.length.toShort) ++ BigInt(Long.MaxValue).toByteArray, DataType.BigInteger), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(Long.MaxValue), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(Int.MaxValue), DataType.Int32), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Timestamp))
     )
     BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Amount), TimestampDataTypeObj) should be (
       Right(DataEntry(Longs.toByteArray(Int.MaxValue), DataType.Timestamp))
     )
   }
  property("test convertion of all numberical types to BigInt") {
    BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Amount), BigIntDataTypeObj) should be (
      Right(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger))
    )
    BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(1000), DataType.Int32), BigIntDataTypeObj) should be (
      Right(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger))
    )
    BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(1000), DataType.Timestamp), BigIntDataTypeObj) should be (
      Right(DataEntry(Shorts.toByteArray(BigInt(1000).toByteArray.length.toShort) ++ BigInt(1000).toByteArray, DataType.BigInteger))
    )
  }
  property("test overflow to Int32") {
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Int.MaxValue) + 1).toByteArray.length.toShort) ++ (BigInt(Int.MaxValue) + 1).toByteArray, DataType.BigInteger), Int32DataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Int.MaxValue + 1), DataType.Amount), Int32DataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(Int.MaxValue + 1), DataType.Timestamp), Int32DataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray.length.toShort) ++ (BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray, DataType.BigInteger), Int32DataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
  }
  property("test overflow to Amount") {
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Long.MaxValue) + 1).toByteArray.length.toShort) ++ (BigInt(Long.MaxValue) + 1).toByteArray, DataType.BigInteger), AmountDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry((BigInt(Long.MaxValue) + 1).toByteArray, DataType.Amount), AmountDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry((BigInt(Long.MaxValue) + 1).toByteArray, DataType.Timestamp), AmountDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray.length.toShort) ++ (BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray, DataType.BigInteger), AmountDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
  }
  property("test overflow to Timestamp") {
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Long.MaxValue) + 1).toByteArray.length.toShort) ++ (BigInt(Long.MaxValue) + 1).toByteArray, DataType.BigInteger), TimestampDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry((BigInt(Long.MaxValue) + 1).toByteArray, DataType.Amount), TimestampDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry((BigInt(Long.MaxValue) + 1).toByteArray, DataType.Timestamp), TimestampDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
    BasicOpcDiff.convertion(DataEntry(Shorts.toByteArray((BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray.length.toShort) ++ (BigInt(Long.MaxValue) * BigInt(Long.MaxValue)).toByteArray, DataType.BigInteger), TimestampDataTypeObj) should be (
      Left(ValidationError.OverflowError)
    )
  }
  property("test different length numbers for conversion to BigInt") {
    val l: List[Long] = List(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
      1000000000, 10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
      1000000000000000L, 10000000000000000L, 100000000000000000L, 1000000000000000000L)
    for (a <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(a), DataType.Amount), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(a).toByteArray.length.toShort) ++ BigInt(a).toByteArray, DataType.BigInteger))
      )
    }
    for (b <- l.slice(0, 9)) {
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(b.toInt), DataType.Int32), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(b).toByteArray.length.toShort) ++ BigInt(b).toByteArray, DataType.BigInteger))
      )
    }
    for (c <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(c), DataType.Timestamp), BigIntDataTypeObj) should be(
        Right(DataEntry(Shorts.toByteArray(BigInt(c).toByteArray.length.toShort) ++ BigInt(c).toByteArray, DataType.BigInteger))
      )
    }
  }

  property("test different length negative numbers for conversion to BigInt") {
    val l: List[Long] = List(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000,
      1000000000, 10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
      1000000000000000L, 10000000000000000L, 100000000000000000L, 1000000000000000000L)
    for (a <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(-a), DataType.Amount), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(-a).toByteArray.length.toShort) ++ BigInt(-a).toByteArray, DataType.BigInteger))
      )
    }
    for (b <- l.slice(0, 9)) {
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(-b.toInt), DataType.Int32), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(-b).toByteArray.length.toShort) ++ BigInt(-b).toByteArray, DataType.BigInteger))
      )
    }
    for (c <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(-c), DataType.Timestamp), BigIntDataTypeObj) should be(
        Right(DataEntry(Shorts.toByteArray(BigInt(-c).toByteArray.length.toShort) ++ BigInt(-c).toByteArray, DataType.BigInteger))
      )
    }
  }

  property("test different array length numbers for conversion to BigInt") {
    val l: List[Long] = List(Math.pow(2, 0).toLong, Math.pow(2, 8).toLong, Math.pow(2, 16).toLong, Math.pow(2, 24).toLong,
      (Math.pow(2, 32)/2 - 1).toLong, Math.pow(2, 32).toLong, Math.pow(2, 40).toLong, Math.pow(2, 48).toLong,
      Math.pow(2, 56).toLong, (Math.pow(2, 64)/2 - 1).toLong)
    for (a <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(a), DataType.Amount), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(a).toByteArray.length.toShort) ++ BigInt(a).toByteArray, DataType.BigInteger))
      )
    }
    for (b <- l.slice(0, 4)) {
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(b.toInt), DataType.Int32), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(b).toByteArray.length.toShort) ++ BigInt(b).toByteArray, DataType.BigInteger))
      )
    }
    for (c <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(c), DataType.Timestamp), BigIntDataTypeObj) should be(
        Right(DataEntry(Shorts.toByteArray(BigInt(c).toByteArray.length.toShort) ++ BigInt(c).toByteArray, DataType.BigInteger))
      )
    }
  }

  property("test different array length negative numbers for conversion to BigInt") {
    val l: List[Long] = List(Math.pow(2, 0).toLong, Math.pow(2, 8).toLong, Math.pow(2, 16).toLong, Math.pow(2, 24).toLong,
      (Math.pow(2, 32)/2 - 1).toLong, Math.pow(2, 32).toLong, Math.pow(2, 40).toLong, Math.pow(2, 48).toLong,
      Math.pow(2, 56).toLong, (Math.pow(2, 64)/2 - 1).toLong)
    for (a <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(-a), DataType.Amount), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(-a).toByteArray.length.toShort) ++ BigInt(-a).toByteArray, DataType.BigInteger))
      )
    }
    for (b <- l.slice(0, 4)) {
      BasicOpcDiff.convertion(DataEntry(Ints.toByteArray(-b.toInt), DataType.Int32), BigIntDataTypeObj) should be (
        Right(DataEntry(Shorts.toByteArray(BigInt(-b).toByteArray.length.toShort) ++ BigInt(-b).toByteArray, DataType.BigInteger))
      )
    }
    for (c <- l) {
      BasicOpcDiff.convertion(DataEntry(Longs.toByteArray(-c), DataType.Timestamp), BigIntDataTypeObj) should be(
        Right(DataEntry(Shorts.toByteArray(BigInt(-c).toByteArray.length.toShort) ++ BigInt(-c).toByteArray, DataType.BigInteger))
      )
    }
  }
}
