package vsys.blockchain.transaction

import vsys.account.Address
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.ValidationError.InvalidSignature
import vsys.utils.serialization.{BytesSerializable, JsonSerializable}

trait Transaction extends BytesSerializable with JsonSerializable with Signed {
  val id: ByteStr

  val transactionType: TransactionType.TxTypeVal

  val transactionFee: Long

  val feeScale: Short

  val timestamp: Long

  override def toString: String = json.toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id == tx.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

trait Signed {
  protected def signatureValid: Boolean

  def signedDescendants: Seq[Signed] = Seq.empty
}

object Signed {

  type E[A] = Either[InvalidSignature, A]

  def validateSignatures[S <: Signed](s: S): E[S] =
    if (!s.signatureValid) Left(InvalidSignature(s, None))
    else s.signedDescendants.par.find { descendant =>
      validateSignatures(descendant).isLeft
    }.fold[E[S]](Right(s))(sd => Left(InvalidSignature(s, Some(validateSignatures(sd).left.get))))
}

trait NonFeeTransaction extends Transaction {
  val transactionFee: Long = 0
  val feeScale: Short = 100
}

trait AmountInvoved {
  val amount: Long
  val recipient: Address // TODO: could be `Account` ?
}
