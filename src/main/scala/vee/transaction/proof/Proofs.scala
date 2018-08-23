package vee.transaction.proof

import com.wavesplatform.state2._
import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{TransactionParser, ValidationError}

import scala.util.Try

case class Proofs(proofs: List[Proof]) {
  val bytes: Array[Byte] = Proofs.Version +: Deser.serializeArrays(proofs.map(_.bytes.arr))
  val base58: Seq[String] = proofs.map(p => Base58.encode(p.bytes.arr))
}

object Proofs {

  val Version            = 1: Byte
  val MaxProofs          = 1
  val MaxProofSize       = 64 + 32 + 1
  // signature length = 64, public-key length = 32, proof type length = 1
  val MaxProofStringSize = base58Length(MaxProofSize) + TransactionParser.KeyStringLength + base58Length(2)

  lazy val empty = Proofs(List.empty)

  def toProofs(proofsByteStr: List[ByteStr]): Either[ValidationError, List[Proof]] = {
    val p = proofsByteStr.map(f => f.arr.headOption match {
      case None => Left(ValidationError.InvalidProofType)
      case Some(b) =>
        ProofType.fromByte(b) match {
          case Some(proofType) if proofType == ProofType.Curve25519 =>
            EllipticCurve25519Proof.fromBytes(f.arr) match {
              case Left(left) => Left(left)
              case Right(right) => Right(EllipticCurve25519Proof.toProof(right))
            }
          case _ => Left(ValidationError.InvalidProofType)
        }
    })

    val r = p collectFirst { case Left(f) => f } toLeft {
      p collect {case Right(right) => right}
    }
    r
  }

  def verifyProofs(toVerify: Array[Byte], proofs: Proofs): Boolean =
    proofs.proofs.forall(f => Proof.verifyProof(toVerify, f))

  def create(proofsByteStr: List[ByteStr]): Either[ValidationError, Proofs] =
    for {
      _ <- Either.cond(proofsByteStr.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
      _ <- Either.cond(!proofsByteStr.map(_.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
      _ <- Either.cond(!proofsByteStr.map(_.arr.head).exists(ProofType.fromByte(_).get != ProofType.Curve25519), (), ValidationError.InvalidProofType)
      proofs <- toProofs(proofsByteStr)
    } yield Proofs(proofs)

  def fromBytes(ab: Array[Byte]): Either[ValidationError, Proofs] =
    for {
      _    <- Either.cond(ab.headOption contains 1, (), GenericError(s"Proofs version must be 1, actual:${ab.headOption}"))
      arrs <- Try(Deser.parseArrays(ab.tail)).toEither.left.map(er => GenericError(er.toString))
      r <- create(arrs.map(ByteStr(_)).toList)
    } yield r
}
