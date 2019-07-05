package vsys.blockchain.contract

import vsys.account.{ContractAccount, PublicKeyAccount}
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.{ProvenTransaction, ValidationError}
import vsys.blockchain.transaction.ValidationError.{InvalidContractAddress, InvalidFunctionIndex}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.utils.serialization.Deser

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
    val opcFunc = tx.contract.trigger.find(a => (a.length > 2) && (a(2) == 0.toByte)).getOrElse(Array[Byte]())
    val stateVar = tx.contract.stateVar
    val description = Deser.serilizeString(tx.description)
    Right(ExecutionContext(signers, s, height, tx, contractId, opcFunc, stateVar, description))
  }

  def fromExeConTx(s: StateReader,
                   height: Int,
                   tx: ExecuteContractFunctionTransaction): Either[ValidationError, ExecutionContext] = {
    val signers = tx.proofs.proofs.map(x => EllipticCurve25519Proof.fromBytes(x.bytes.arr).toOption.get.publicKey)
    val contractId = tx.contractId
    val description = tx.attachment
    s.contractContent(tx.contractId.bytes) match {
      case Some(c) if tx.funcIdx >=0 && tx.funcIdx < c._3.descriptor.length => Right(ExecutionContext(signers, s, height, tx, contractId, c._3.descriptor(tx.funcIdx), c._3.stateVar, description))
      case Some(_) => Left(InvalidFunctionIndex)
      case _ => Left(InvalidContractAddress)
    }
  }

}
