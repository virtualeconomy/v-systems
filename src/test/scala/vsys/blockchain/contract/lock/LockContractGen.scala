package vsys.blockchain.contract.lock

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractLock, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait LockContractGen {

  val lockId: Short = 0

  def lockContractGen(): Gen[Contract] =
    ContractLock.contract

  def initLockContractDataStackGen(tokenId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry(tokenId, DataType.TokenId))
  } yield Seq(tokenId)

  def lockFunctionDataStackGen(lockTime: Long): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry(Longs.toByteArray(lockTime), DataType.Timestamp))
  } yield Seq(tokenId)

  def genesisLockGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerLockGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                                                   description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def lockAssetGen(signer: PrivateKeyAccount, contractId: ContractAccount, lockTime: Long,
                   attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- lockFunctionDataStackGen(lockTime)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, lockId, data, attachment, fee, feeScale, ts).explicitGet()

}
