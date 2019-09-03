package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.account.Address
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.utils.crypto.hash.FastCryptographicHash

import scala.util.{Left, Right}

object AssertOpcDiff {

  def gtEq0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) >= 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gteq0): Value ${Longs.fromByteArray(v.data)} is negative"))
  }

  def ltEq(v1: DataEntry, v2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v1.dataType == DataType.Amount && v2.dataType == DataType.Amount
      && Longs.fromByteArray(v1.data) <= Longs.fromByteArray(v2.data))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (lteq0): Value ${Longs.fromByteArray(v2.data)} is larger than $v1"))
  }

  def ltInt64(m: DataEntry): Either[ValidationError, OpcDiff] = {
    if (m.dataType == DataType.Amount && Longs.fromByteArray(m.data) <= Long.MaxValue)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (ltint64): Value ${Longs.fromByteArray(m.data)} is invalid"))
  }

  def gt0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) > 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gt0): Value $v is non-positive"))
  }

  def eq(add1: DataEntry, add2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (add1.dataType == DataType.Address && add2.dataType == DataType.Address
      && Address.fromBytes(add1.data) == Address.fromBytes(add2.data))
      Right(OpcDiff.empty)
    else if (add1.dataType == DataType.Amount && add2.dataType == DataType.Amount
      && Longs.fromByteArray(add1.data) == Longs.fromByteArray(add2.data))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): DataEntry ${add1.data} is not equal to ${add2.data}"))
  }

  def isCallerOrigin(context: ExecutionContext)(address: DataEntry): Either[ValidationError, OpcDiff] = {
    val signer = context.signers.head
    if (address.dataType != DataType.Address)
      Left(ContractDataTypeMismatch)
    else if (!(address.data sameElements signer.bytes.arr))
      Left(ContractInvalidCaller)
    else
      Right(OpcDiff.empty)
  }

  def isSignerOrigin(context: ExecutionContext)(address: DataEntry): Either[ValidationError, OpcDiff] = {
    val signer = context.signers.head
    if (address.dataType != DataType.Address)
      Left(ContractDataTypeMismatch)
    else if (!(address.data sameElements signer.bytes.arr))
      Left(ContractInvalidSigner)
    else
      Right(OpcDiff.empty)
  }

  def checkHash(hashValue: DataEntry, hashKey: DataEntry): Either[ValidationError, OpcDiff] = {
    if (hashValue.dataType != DataType.ShortText || hashKey.dataType != DataType.ShortText)
      Left(ContractDataTypeMismatch)
    else {
      val hashResult = ByteStr(FastCryptographicHash(hashKey.data))
      if (hashResult.equals(ByteStr(hashValue.data)))
        Right(OpcDiff.empty)
      else
        Left(ContractInvalidHash)
    }
  }

  object AssertType extends Enumeration(1) {
    val GteqZeroAssert, LteqAssert, LtInt64Assert, GtZeroAssert, EqAssert, IsCallerOriginAssert, IsSignerOriginAssert = Value
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    if (checkAssertDataIndex(bytes, data.length)) {
      val gteqZeroAssertId = AssertType.GteqZeroAssert.id.toByte
      val lteqAssertId = AssertType.LteqAssert.id.toByte
      val ltInt64AssertId = AssertType.LtInt64Assert.id.toByte
      val gtZeroAssertId = AssertType.GtZeroAssert.id.toByte
      val eqAssertId = AssertType.EqAssert.id.toByte
      val isCallerOriginAssertId = AssertType.IsCallerOriginAssert.id.toByte
      val isSignerOriginAssertId = AssertType.IsSignerOriginAssert.id.toByte
      (bytes.headOption, bytes.length) match {
        case (Some(`gteqZeroAssertId`), 2) => gtEq0(data(bytes(1)))
        case (Some(`lteqAssertId`), 3) => ltEq(data(bytes(1)), data(bytes(2)))
        case (Some(`ltInt64AssertId`), 2) => ltInt64(data(bytes(1)))
        case (Some(`gtZeroAssertId`), 2) => gt0(data(bytes(1)))
        case (Some(`eqAssertId`), 3) => eq(data(bytes(1)), data(bytes(2)))
        case (Some(`isCallerOriginAssertId`), 2) => isCallerOrigin(context)(data(bytes(1)))
        case (Some(`isSignerOriginAssertId`), 2) => isSignerOrigin(context)(data(bytes(1)))
        case _ => Left(ContractInvalidOPCData)
      }
    }
    else
      Left(ContractInvalidOPCData)
  }

  private def checkAssertDataIndex(bytes: Array[Byte], dataLength: Int): Boolean =
    bytes.tail.max < dataLength && bytes.tail.min >= 0

}

