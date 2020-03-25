package vsys.blockchain.contract.channel

import org.scalacheck.Gen
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait PaymentChannelContractGen {

  val createIndex: Short = 0
  val updateExpiredTimeIndex: Short = 1
  val chargeIndex: Short = 2
  val terminateIndex: Short = 3
  val executeWithdrawIndex: Short = 4
  val executePaymentIndex: Short = 5

  def paymentChannelContractGen(): Gen[Contract] =
    ContractPaymentChannel.contract

  def initPaymentChannelContractDataStackGen(tokenId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry(tokenId, DataType.TokenId))
  } yield Seq(tokenId)

  def channelIdDataStackGen(channelId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    id <- Gen.const(DataEntry.create(channelId, DataType.ShortBytes).right.get)
  } yield Seq(id)

  def genesisPaymentChannelGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerPaymentChannelGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                         description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def createChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = createIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def updateExpiredTimeChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                                  attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = updateExpiredTimeIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def chargeChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = chargeIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def terminateChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, channelId: Array[Byte],
                          attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = terminateIndex.toShort
    for {
      data: Seq[DataEntry] <- channelIdDataStackGen(channelId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def executeWithdrawChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, channelId: Array[Byte],
                                attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = executeWithdrawIndex.toShort
    for {
      data: Seq[DataEntry] <- channelIdDataStackGen(channelId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def executePaymentChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                               attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = executePaymentIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }
}
