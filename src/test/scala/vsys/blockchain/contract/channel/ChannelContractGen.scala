package vsys.blockchain.contract.channel

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractPaymentChannel, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait ChannelContractGen {

  val createIndex: Short = 0
  val updateExpiredTimeIndex: Short = 1
  val chargeIndex: Short = 2
  val terminateIndex: Short = 3

  def initChannelDataStackGen(tokenId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    tId <- Gen.const(DataEntry(tokenId, DataType.TokenId))
  } yield Seq(tId)

  def createFunctionDataStackGen(recipient: Address, amount: Long, expiredTime: Long): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    ts <- Gen.const(DataEntry(Longs.toByteArray(expiredTime), DataType.Timestamp))
  } yield Seq(reci, am, ts)

  def updateExpiredTimeFunctionDataStackGen(channelId: String, expiredTime: Long): Gen[Seq[DataEntry]] = for {
    cId <- Gen.const(DataEntry(channelId.getBytes(), DataType.ShortText))
    ts <- Gen.const(DataEntry(Longs.toByteArray(expiredTime), DataType.Timestamp))
  } yield Seq(cId, ts)

  def chargeFunctionDataStackGen(channelId: String, amount: Long): Gen[Seq[DataEntry]] = for {
    cId <- Gen.const(DataEntry(channelId.getBytes(), DataType.ShortText))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(cId, am)

  def terminateFunctionDataStackGen(channelId: String): Gen[Seq[DataEntry]] = for {
    cId <- Gen.const(DataEntry(channelId.getBytes(), DataType.ShortText))
  } yield Seq(cId)

  def executePaymentFunctionDataStackGen(channelId: String, amount: Long, signature: String): Gen[Seq[DataEntry]] = for {
    cId <- Gen.const(DataEntry(channelId.getBytes(), DataType.ShortText))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    sig <- Gen.const(DataEntry(signature.getBytes(), DataType.ShortText))
  } yield Seq(cId, am, sig)

  def paymenChannelContractGen(): Gen[Contract] =
    ContractPaymentChannel.contract

  def registerChannelGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                         description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def genesisChannelGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def createChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, recipient: Address, amount: Long, expiredTime: Long,
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- createFunctionDataStackGen(recipient, amount, expiredTime)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, createIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def updateExpiredTimeGen(signer: PrivateKeyAccount, contractId: ContractAccount, channelId: String, expiredTime: Long,
                           attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- updateExpiredTimeFunctionDataStackGen(channelId, expiredTime)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, updateExpiredTimeIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def chargeGen(signer: PrivateKeyAccount, contractId: ContractAccount, channelId: String, amount: Long,
                attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- chargeFunctionDataStackGen(channelId, amount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, chargeIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def terminateGen(signer: PrivateKeyAccount, contractId: ContractAccount, channelId: String,
                   attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- terminateFunctionDataStackGen(channelId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, terminateIndex, data, attachment, fee, feeScale, ts).explicitGet()
  }
}