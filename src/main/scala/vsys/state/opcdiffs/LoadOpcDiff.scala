package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError
import vsys.contract.{ExecutionContext, DataEntry, DataType}
import vsys.transaction.proof.EllipticCurve25519Proof

import scala.util.Right

object LoadOpcDiff {

  def issuer(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val issuer = context.state.contractInfo(context.contractId.bytes).get
    val newDataStack = dataStack :+ issuer
    Right(newDataStack)
  }

  def sender(context: ExecutionContext)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val sender = EllipticCurve25519Proof.fromBytes(context.transaction.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val newDataStack = dataStack :+ DataEntry(sender.bytes.arr, DataType.Address)
    Right(newDataStack)
  }

  def max(context: ExecutionContext)(tokenIdx: DataEntry,
                                          dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array("max".toByte)))
    val max = context.state.tokenInfo(id).get
    val newDataStack = dataStack :+ max
    Right(newDataStack)
  }

  def total(context: ExecutionContext)(tokenIdx: DataEntry,
                                            dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, Array("total".toByte)))
    val total = context.state.tokenAccountBalance(id).get
    val newDataStack = dataStack :+ DataEntry(Longs.toByteArray(total), DataType.Amount)
    Right(newDataStack)
  }

  def balance(context: ExecutionContext)(addr: DataEntry,
                                              tokenIdx: DataEntry,
                                              dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val id: ByteStr = ByteStr(Bytes.concat(context.contractId.bytes.arr, tokenIdx.data, addr.data))
    val balance = context.state.tokenAccountBalance(id).get
    val newDataStack = dataStack :+ DataEntry(Longs.toByteArray(balance), DataType.Amount)
    Right(newDataStack)
  }

  object LoadType extends Enumeration {
    val IssuerLoad = Value(1)
    val SenderLoad = Value(2)
    val MaxLoad = Value(3)
    val TotalLoad = Value(4)
    val BalanceLoad = Value(5)
  }

}


