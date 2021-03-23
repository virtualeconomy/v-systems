package vsys.blockchain.contract.token

import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.{Contract, ContractTokenV2, DataEntry, DataType}
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction

trait TokenContractV2Gen extends TokenContractGen {

  val updateListIndex: Short = 3

  def updateListDataStackGen(user: Address, value: Boolean): Gen[Seq[DataEntry]] = for {
    u <- Gen.const(DataEntry(user.bytes.arr, DataType.Address))
    b: Byte = if (value) 1.toByte else 0.toByte
    v <- Gen.const(DataEntry(Array(b), DataType.Boolean))
  } yield Seq(u, v)

  def tokenContractV2Gen(white: Boolean): Gen[Contract] =
    if (white) ContractTokenV2.contractTokenWhiteList
    else ContractTokenV2.contractTokenBlackList

  def updateListTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, user: Address, value: Boolean,
                         attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- updateListDataStackGen(user, value)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, updateListIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def sendTokenGen(sender: PrivateKeyAccount, contractId: ContractAccount, rep: Address, amount: Long,
                            attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- sendDataStackGen(rep, amount)
  } yield ExecuteContractFunctionTransaction.create(sender, contractId, sendIndex, data, attachment, fee, feeScale, ts).explicitGet()

}
