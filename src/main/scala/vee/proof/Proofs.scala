package vee.proof

import com.wavesplatform.state2._
import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{TransactionParser, ValidationError}

import scala.util.{Failure, Success, Try}

case class Proofs(proofs: Seq[Proof]) {
  val bytes: Array[Byte] = Proofs.Version +: Deser.serializeArrays(proofs.map(_.bytes.arr))
  val base58: Seq[String] = proofs.map(p => Base58.encode(p.bytes.arr))
}

object Proofs {

  val Version            = 1: Byte
  val MaxProofs          = 8
  val MaxProofSize       = 64
  // signature length = 64, publickey length = 32 + 1, proof type length = 1
  val MaxProofStringSize = base58Length(MaxProofSize) + TransactionParser.KeyStringLength + base58Length(2)

  lazy val empty = create(Seq.empty).explicitGet()

  def create(proofs: Seq[Proof]): Either[ValidationError, Proofs] =
    for {
      _ <- Either.cond(proofs.lengthCompare(MaxProofs) <= 0, (), GenericError(s"Too many proofs, max $MaxProofs proofs"))
      _ <- Either.cond(!proofs.map(_.bytes.arr.length).exists(_ > MaxProofSize), (), GenericError(s"Too large proof, must be max $MaxProofSize bytes"))
    } yield Proofs(proofs)

  def fromBytes(ab: Array[Byte]): Either[ValidationError, Proofs] =
    for {
      _    <- Either.cond(ab.headOption contains 1, (), GenericError(s"Proofs version must be 1, actual:${ab.headOption}"))
      arrs <- Try(Deser.parseArrays(ab.tail)).toEither.left.map(er => GenericError(er.toString))
      r    <- create(arrs.map(Proof.fromBytes(_).fold(left => Failure(new Exception(left.toString)), right => Success(right)).get))
    } yield r
}
