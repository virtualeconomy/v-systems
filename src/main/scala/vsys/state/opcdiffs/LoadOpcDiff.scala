package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import vsys.contract.{DataEntry, DataType}
import vsys.transaction.ProvenTransaction
import vsys.transaction.contract.ExecuteContractFunctionTransaction
import vsys.transaction.proof.EllipticCurve25519Proof

import scala.util.Right

object LoadOpcDiff {

  def issuer(s: StateReader,
             tx: ExecuteContractFunctionTransaction)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val issuer = s.contractInfo(tx.contractId.bytes).get
    val newDataStack = dataStack :+ issuer
    Right(newDataStack)
  }

  def sender(tx: ProvenTransaction)(dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val newDataStack = dataStack :+ DataEntry(sender.bytes.arr, DataType.Address)
    Right(newDataStack)
  }

  def max(s: StateReader,
          tx: ExecuteContractFunctionTransaction)(tokenIdx: DataEntry,
                                          dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val contractId = tx.contractId.bytes.arr
    val id: ByteStr = ByteStr(Bytes.concat(contractId, tokenIdx.data, Array("max".toByte)))
    val max = s.tokenInfo(id).get
    val newDataStack = dataStack :+ max
    Right(newDataStack)
  }

  def total(s: StateReader,
            tx: ExecuteContractFunctionTransaction)(tokenIdx: DataEntry,
                                            dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val contractId = tx.contractId.bytes.arr
    val id: ByteStr = ByteStr(Bytes.concat(contractId, tokenIdx.data, Array("total".toByte)))
    val total = s.tokenAccountBalance(id).get
    val newDataStack = dataStack :+ DataEntry(Longs.toByteArray(total), DataType.Amount)
    Right(newDataStack)
  }

  def balance(s: StateReader,
              tx: ExecuteContractFunctionTransaction)(addr: DataEntry,
                                              tokenIdx: DataEntry,
                                              dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    val contractId = tx.contractId.bytes.arr
    val id: ByteStr = ByteStr(Bytes.concat(contractId, tokenIdx.data, addr.data))
    val balance = s.tokenAccountBalance(id).get
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


