package vsys.contract

import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.serialization.Deser
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.account.ContractAccount
import vsys.transaction.ProvenTransaction
import vsys.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.transaction.proof.EllipticCurve25519Proof

case class ExecutionContext(signers: Seq[PublicKeyAccount],
                            state: StateReader,
                            height: Int,
                            transaction: ProvenTransaction,
                            contractId: ContractAccount,
                            opcFunc: Array[Byte],
                            stateVar: Seq[Array[Byte]],
                            description: Array[Byte]) {
  
}

object ExecutionContext {

  def fromRegConTx(s: StateReader,
                   height: Int,
                   tx: RegisterContractTransaction): Either[ValidationError, ExecutionContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val opcFunc = tx.contract.initializer
    val stateVar = tx.contract.stateVar
    val description = Deser.serilizeString(tx.description)
    Right(ExecutionContext(signers, s, height, tx, contractId, opcFunc, stateVar, description))
  }

  def fromExeConTx(s: StateReader,
                   height: Int,
                   tx: ExecuteContractFunctionTransaction): Either[ValidationError, ExecutionContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val description = tx.description
    s.contractContent(tx.contractId.bytes) match {
      case Some(c) => if ((c._3.descriptor.length == 12) && (tx.funcIdx > 4)) Left(GenericError(s"Invalid function index"))
      else if ((c._3.descriptor.length == 11) && (tx.funcIdx > 3)) Left(GenericError(s"Invalid function index"))
      else Right(ExecutionContext(signers, s, height, tx, contractId, c._3.descriptor(tx.funcIdx), c._3.stateVar, description))
      case _ => Left(GenericError(s"Invalid contract id"))
    }
  }

}
