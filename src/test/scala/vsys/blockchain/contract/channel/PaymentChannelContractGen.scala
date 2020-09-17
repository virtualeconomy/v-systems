package vsys.blockchain.contract.channel

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}

trait PaymentChannelContractGen extends SystemContractGen
  with TokenContractGen {

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

  def createChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = createIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def updateExpiredTimeChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                  attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = updateExpiredTimeIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def chargeChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
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

  def executePaymentChannelGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                               attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = executePaymentIndex.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  val channelContract: Gen[Contract] = paymentChannelContractGen()

  def createAndDepositVSYSPaymentChannelGen(depositValue: Long, channelCapacity: Long, channelExpiredTime: Long): Gen[(GenesisTransaction,
    GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long, String, Array[Byte])] = for {
      (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
      genesis <- genesisPaymentChannelGen(master, ts)
      user <- accountGen
      genesis2 <- genesisPaymentChannelGen(user, ts)
      contract <- channelContract
      description <- validDescStringGen
      sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
      dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
      // Register a payment channel that supports VSYS
      regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
      contractId = regContract.contractId
      attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
      depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(depositValue))
      depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
      depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
      createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(channelCapacity), Longs.toByteArray(ts + channelExpiredTime))
      createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
      create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
  } yield (genesis, genesis2, master, user, regContract, depositVSYS, create, ts, fee, description, attach)

  val tokenContract: Gen[Contract] = tokenContractGen(false)

  def createAndDepositTokenPaymentChannelGen(totalSupply: Long, unity: Long, issueAmount: Long, depositValue: Long, channelCapacity: Long, channelExpiredTime: Long): Gen[(GenesisTransaction,
    GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long, String, Array[Byte])] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    genesis2 <- genesisPaymentChannelGen(user, ts)
    tContract <- tokenContract
    cContract <- channelContract
    description <- validDescStringGen
    initDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    regTokenContract <- registerTokenGen(master, tContract, initDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(tokenId.arr)
    // register a payment channel contract only support regTokenContract's' tokenId
    regContract <- registerPaymentChannelGen(master, cContract, dataStack, description, fee + 10000000000L, ts + 1)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(master, tokenContractId, issueAmount, attach, fee, ts + 1)
    // use token contract deposit/withdraw
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(depositValue))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 2)
    //create payment channel
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(channelCapacity), Longs.toByteArray(ts + channelExpiredTime))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts + 3)
  } yield (genesis, genesis2, master, user, regTokenContract, regContract, issueToken, depositToken, create, ts, fee, description, attach)
}
