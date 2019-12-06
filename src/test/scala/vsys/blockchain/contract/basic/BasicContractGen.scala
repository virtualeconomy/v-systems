package vsys.blockchain.contract.basic

import org.scalacheck.Gen
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{Contract, ContractDepositWithdrawProductive, ContractDepositWithdraw, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.RegisterContractTransaction

trait BasicContractGen {

  def contractGen(productive: Boolean): Gen[Contract] =
    if (productive) ContractDepositWithdrawProductive.contract
    else ContractDepositWithdraw.contract

  def initDepositWithdrawDataStackGen(tokenId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry(tokenId, DataType.TokenId))
  } yield Seq(tokenId)

  def genesisBasicGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerDepositWithdrawContractGen(signer: PrivateKeyAccount, contract: Contract, description: String,
                                         fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, Seq(), description, fee, feeScale, ts).explicitGet()

  def registerDepositWithdrawProductiveContractGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                                                   description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

}
