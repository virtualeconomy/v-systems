package vsys.blockchain.contract.token

import com.google.common.primitives.Ints
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper.{ENOUGH_AMT, feeScale}
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractNonFungibleV2, DataEntry, DataType}
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait NonFungibleContractV2Gen {
  val supersedeIndex: Short = 0
  val issueIndex: Short = 1
  val updateListIndex: Short = 2
  val sendIndex: Short = 3
  val transferIndex: Short = 4
  val depositIndex: Short = 5
  val withdrawIndex: Short = 6

  def initTokenDataStackGen(): Gen[Seq[DataEntry]] = Seq()

  def issueDataStackGen(desc: String): Gen[Seq[DataEntry]] = for {
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(shortText)

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    add <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(add)

  def sendDataStackGen(recipient: Address, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    toIdx <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(reci, toIdx)

  def nonFungibleContractWhiteGen(): Gen[Contract] = ContractNonFungibleV2.contractNFTWhitelist

  def nonFungibleContractBlackGen(): Gen[Contract] = ContractNonFungibleV2.contractNFTBlacklist

  def registerNonFungibleV2Gen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                             description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def genesisNonFungibleV2Gen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def supersedeNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, newAdd: Address,
                              attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- addressDataStackGen(newAdd)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, supersedeIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def issueNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, desc: String,
                          attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- issueDataStackGen(desc)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, issueIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def updateListNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                               attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = updateListIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def sendNonFungibleV2Gen(sender: PrivateKeyAccount, contractId: ContractAccount, rep: Address, toIdx: Int,
                         attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = sendIndex
    for {
      data: Seq[DataEntry] <- sendDataStackGen(rep, toIdx)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def transferNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                             attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = transferIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def depositNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                            attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = depositIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def withdrawNonFungibleV2Gen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                             attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = withdrawIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }
}
