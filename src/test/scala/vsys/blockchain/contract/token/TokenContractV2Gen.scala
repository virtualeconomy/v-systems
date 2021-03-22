package vsys.blockchain.contract.token

import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.{Contract, ContractTokenV2, DataEntry}
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction

trait TokenContractV2Gen extends TokenContractGen {

  val updateListIndex: Short = 3

  def tokenContractV2Gen(white: Boolean): Gen[Contract] =
    if (white) ContractTokenV2.contractTokenWhiteList
    else ContractTokenV2.contractTokenBlackList

  def updateListTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, user: Address,
                         attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- addressDataStackGen(user)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, updateListIndex, data, attachment, fee, feeScale, ts).explicitGet()

}
