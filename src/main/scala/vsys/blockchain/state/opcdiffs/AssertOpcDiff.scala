package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.account.Address
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.utils.crypto.hash.FastCryptographicHash

import scala.util.{Left, Right, Try}

object AssertOpcDiff extends OpcDiffer {

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

  def equal(add1: DataEntry, add2: DataEntry): Either[ValidationError, OpcDiff] = {
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
      Either.cond(hashResult.equals(ByteStr(hashValue.data)), OpcDiff.empty, ContractInvalidHash)
    }
  }

  object AssertType extends Enumeration {
    sealed case class AssertTypeVal(
      assertType: Int,
      operandCount: Int,
      differ: (ExecutionContext, Array[Byte], Seq[DataEntry]) => Either[ValidationError, OpcDiff])
    extends Val(assertType) { def *(n: Int): Int = n * assertType }

    val GteqZeroAssert       = AssertTypeVal(1, 1, (c, b, d) => gtEq0(d(b(1))))
    val LteqAssert           = AssertTypeVal(2, 2, (c, b, d) => ltEq(d(b(1)), d(b(2))))
    val LtInt64Assert        = AssertTypeVal(3, 1, (c, b, d) => ltInt64(d(b(1))))
    val GtZeroAssert         = AssertTypeVal(4, 1, (c, b, d) => gt0(d(b(1))))
    val EqAssert             = AssertTypeVal(5, 2, (c, b, d) => equal(d(b(1)), d(b(2))))
    val IsCallerOriginAssert = AssertTypeVal(6, 1, (c, b, d) => isCallerOrigin(c)(d(b(1))))
    val IsSignerOriginAssert = AssertTypeVal(7, 1, (c, b, d) => isSignerOrigin(c)(d(b(1))))

    def fromByte(b: Byte): Option[AssertTypeVal] = Try(AssertType(b).asInstanceOf[AssertTypeVal]).toOption
  }

  // index out of bound exception if custom function with this opc in `data(byte(i))`
  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    bytes.headOption.flatMap(AssertType.fromByte(_)) match {
      case Some(t: AssertType.AssertTypeVal) if checkData(bytes, data.length, t.operandCount, false) =>
        t.differ(context, bytes, data)
      case _ => Left(ContractInvalidOPCData)
    }
}

