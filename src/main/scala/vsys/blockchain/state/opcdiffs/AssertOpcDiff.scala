package vsys.blockchain.state.opcdiffs

import scorex.crypto.hash.Sha256
import com.google.common.primitives.Longs
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.utils.crypto.EllipticCurveImpl

import scala.util.{Left, Right, Try}

object AssertOpcDiff extends OpcDiffer {

  def assertTrue(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType != DataType.Boolean)
      Left(ContractDataTypeMismatch)
    else if (v.data sameElements Array(1.toByte))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (Boolean True): Value ${v.json} is False"))
  }

  def gtEq0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType != DataType.Amount)
      Left(ContractDataTypeMismatch)
    else if (Longs.fromByteArray(v.data) >= 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gteq0): Value ${v.json} is negative"))
  }

  def ltEq(v1: DataEntry, v2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v1.dataType != DataType.Amount || v2.dataType != DataType.Amount)
      Left(ContractDataTypeMismatch)
    else if (Longs.fromByteArray(v1.data) <= Longs.fromByteArray(v2.data))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (lteq0): Value ${v1.json} " +
        s"is larger than ${v2.json}"))
  }

  def ltInt64(m: DataEntry): Either[ValidationError, OpcDiff] = {
    if (m.dataType != DataType.Amount)
      Left(ContractDataTypeMismatch)
    else if (Longs.fromByteArray(m.data) <= Long.MaxValue)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (ltint64): Value ${m.json} is invalid"))
  }

  def gt0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType != DataType.Amount)
      Left(ContractDataTypeMismatch)
    else if (Longs.fromByteArray(v.data) > 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gt0): Value ${v.json} is non-positive"))
  }

  def equal(add1: DataEntry, add2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (add1.bytes sameElements add2.bytes)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): DataEntry ${add1.json} is not equal to ${add2.json}"))
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
    if (hashValue.dataType != DataType.ShortBytes || hashKey.dataType != DataType.ShortBytes)
      Left(ContractDataTypeMismatch)
    else {
      val hashResult = ByteStr(Sha256(hashKey.data.tail.tail))
      Either.cond(hashResult.equals(ByteStr(hashValue.data.tail.tail)), OpcDiff.empty, ContractInvalidHash)
    }
  }

  def verifySig(toSign: DataEntry, sig: DataEntry, pub: DataEntry): Either[ValidationError, OpcDiff] = {
    if (toSign.dataType != DataType.ShortBytes || sig.dataType != DataType.ShortBytes || pub.dataType != DataType.PublicKey)
      Left(ContractDataTypeMismatch)
    else {
      Either.cond(EllipticCurveImpl.verify(sig.data.tail.tail, toSign.data.tail.tail, pub.data), OpcDiff.empty, ContractInvalidSignature)
    }
  }

  object AssertType extends Enumeration {
    sealed case class AssertTypeVal(
      assertType: Int,
      operandCount: Int,
      differ: (ExecutionContext, Array[Byte], Seq[DataEntry]) => Either[ValidationError, OpcDiff])
    extends Val(assertType) { def *(n: Int): Int = n * assertType }

    val GteqZeroAssert       = AssertTypeVal(1, 1, (c, b, d)  => gtEq0(d(b(1))))
    val LteqAssert           = AssertTypeVal(2, 2, (c, b, d)  => ltEq(d(b(1)), d(b(2))))
    val LtInt64Assert        = AssertTypeVal(3, 1, (c, b, d)  => ltInt64(d(b(1))))
    val GtZeroAssert         = AssertTypeVal(4, 1, (c, b, d)  => gt0(d(b(1))))
    val EqAssert             = AssertTypeVal(5, 2, (c, b, d)  => equal(d(b(1)), d(b(2))))
    val IsCallerOriginAssert = AssertTypeVal(6, 1, (c, b, d)  => isCallerOrigin(c)(d(b(1))))
    val IsSignerOriginAssert = AssertTypeVal(7, 1, (c, b, d)  => isSignerOrigin(c)(d(b(1))))
    val BooleanTrueAssert    = AssertTypeVal(8, 1, (c, b, d)  => assertTrue(d(b(1))))
    val HashCheckAssert      = AssertTypeVal(9, 2, (c, b, d)  => checkHash(d(b(1)), d(b(2))))
    val SigVerifyAssert      = AssertTypeVal(10, 3, (c, b, d) => verifySig(d(b(1)), d(b(2)), d(b(3))))

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

