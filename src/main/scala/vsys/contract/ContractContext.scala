package vsys.contract

import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError
import vsys.account.ContractAccount
import vsys.transaction.ProvenTransaction
import vsys.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.transaction.proof.EllipticCurve25519Proof

case class ContractContext(signers: Seq[PublicKeyAccount],
                           state: StateReader,
                           height: Int,
                           transaction: ProvenTransaction,
                           contractId: ContractAccount,
                           opcFunc: Array[Byte],
                           description: Array[Byte]) {
  
}

object ContractContext {

  def fromRegConTx(s: StateReader,
                   height: Int,
                   tx: RegisterContractTransaction): Either[ValidationError, ContractContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val opcFunc = s.contractContent(tx.contractId.bytes).get._2.initializer
    val description = tx.description
    Right(ContractContext(signers, s, height, tx, contractId, opcFunc, description))
  }

  def fromExeConTx(s: StateReader,
                   height: Int,
                   tx: ExecuteContractFunctionTransaction): Either[ValidationError, ContractContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val opcFunc = s.contractContent(tx.contractId.bytes).get._2.descriptor(tx.funcIdx)
    val description = tx.description
    Right(ContractContext(signers, s, height, tx, contractId, opcFunc, description))
  }

}
