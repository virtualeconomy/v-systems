package vsys.contract

import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.transaction.{Transaction, ValidationError}
import vsys.account.ContractAccount
import vsys.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.transaction.proof.EllipticCurve25519Proof

sealed trait ContractContext {

  val signers: Seq[PublicKeyAccount]
  val state: StateReader
  val height: Int
  val transaction: Transaction
  val contractId: ContractAccount
  val opcFunc: Array[Byte]
  
}

object ContractContext {

  case class ContractContextImpl(signers: Seq[PublicKeyAccount], state: StateReader,
                                 height: Int, transaction: Transaction,
                                 contractId: ContractAccount, opcFunc: Array[Byte]) extends ContractContext

  def fromRegConTx(s: StateReader,
                   height: Int,
                   tx: RegisterContractTransaction): Either[ValidationError, ContractContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val opcFunc = s.contractContent(tx.contractId.bytes).get._2.initializer
    Right(ContractContextImpl(signers, s, height, tx, contractId, opcFunc))
  }

  def fromExeConTx(s: StateReader,
                   height: Int,
                   tx: ExecuteContractFunctionTransaction): Either[ValidationError, ContractContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val opcFunc = s.contractContent(tx.contractId.bytes).get._2.descriptor(tx.funcIdx)
    Right(ContractContextImpl(signers, s, height, tx, contractId, opcFunc))
  }

}
