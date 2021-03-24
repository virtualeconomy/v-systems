package vsys.blockchain.contract.token

import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractTokenV2, DataEntry, DataType}
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

  def transferTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, transferIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def depositTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, depositIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def withdrawTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, withdrawIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def totalSupplyTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount,
                          attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    ExecuteContractFunctionTransaction.create(signer, contractId, totalSupplyIndex, Nil, attachment, fee, feeScale, ts).explicitGet()
  }

  def maxSupplyTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    ExecuteContractFunctionTransaction.create(signer, contractId, maxSupplyIndex, Nil, attachment, fee, feeScale, ts).explicitGet()
  }

  def balanceOfTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, add: Address,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- addressDataStackGen(add)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, balanceOfIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def getIssuerTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    ExecuteContractFunctionTransaction.create(signer, contractId, getIssuerIndex, Nil, attachment, fee, feeScale, ts).explicitGet()
  }
}
