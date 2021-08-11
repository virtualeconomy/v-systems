package vsys.blockchain.contract.swap

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.Contract
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait AtomicSwapContractGen extends SystemContractGen
  with TokenContractGen {
  val lockIndex: Short = 0
  val solvePuzzleIndex: Short = 1
  val expireWithdrawIndex: Short = 2

  def atomicSwapContractGen: Gen[Contract] =
    ContractAtomicSwap.contract

  def initAtomicSwapContractDataStackGen(tokenId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry(tokenId, DataType.TokenId))
  } yield Seq(tokenId)

  def genesisAtomicSwapGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerAtomicSwapGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                                description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def lockAtomicSwapContractDataStackGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                                         attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = lockIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def solvePuzzleAtomicSwapContractDataStackGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                                                attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = solvePuzzleIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def expireWithdrawAtomicSwapContractDataStackGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                                                   attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = expireWithdrawIndex
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  val atomicSwapContract: Gen[Contract] = atomicSwapContractGen

  def createAndDepositVSYSAtomicSwapContractGen(depositValue: Long): Gen[(GenesisTransaction,
    GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    Long, Long, String, Array[Byte])] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisAtomicSwapGen(master, ts)
    user <- accountGen
    genesis2 <- genesisAtomicSwapGen(user, ts)
    contract <- atomicSwapContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initAtomicSwapContractDataStackGen(sysTokenId.arr)
    // Register an atomic swap contract that swaps VSYS
    regContract <- registerAtomicSwapGen(master, contract, dataStack, description, fee, ts + 1)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(depositValue))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 2)
  } yield (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach)

  val tokenContract: Gen[Contract] = tokenContractGen(false)

  def createAndDepositTokenAtomicSwapContractGen(totalSupply: Long, unity: Long, issueAmount: Long, depositValue: Long): Gen[(GenesisTransaction,
    GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction,
    Long, Long, String, Array[Byte])] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisAtomicSwapGen(master, ts)
    user <- accountGen
    genesis2 <- genesisAtomicSwapGen(user, ts)
    aContract <- atomicSwapContract
    tContract <- tokenContract
    description <- validDescStringGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    regTokenContract <- registerTokenGen(master, tContract, initTokenDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    issueToken <- issueTokenGen(master, tokenContractId, issueAmount, attach, fee, ts + 4)
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initAtomicSwapContractDataStackGen(tokenId.arr)
    // Register an atomic swap contract that swaps Tokens
    regContract <- registerAtomicSwapGen(master, aContract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(depositValue))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 1)
  } yield (genesis, genesis2, master, user, regTokenContract, issueToken, regContract, depositToken, ts, fee, description, attach)

  def createTwoTokenAndDepositSwapGen(totalSupply: Long, unity: Long, issueAmount: Long, depositValue: Long): Gen[(GenesisTransaction,
    GenesisTransaction, PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long, String, Array[Byte])] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisAtomicSwapGen(master, ts)
    user <- accountGen
    genesis2 <- genesisAtomicSwapGen(user, ts)
    tContract <- tokenContract
    aContract <- atomicSwapContract
    description <- validDescStringGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    // Register and deposit first token
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    regTokenContract <- registerTokenGen(master, tContract, initTokenDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    issueToken <- issueTokenGen(master, tokenContractId, issueAmount, attach, fee, ts + 1)
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    swapDataStack <- initAtomicSwapContractDataStackGen(tokenId.arr)
    regSwapContract <- registerAtomicSwapGen(master, aContract, swapDataStack, description, fee, ts + 2)
    swapContractId = regSwapContract.contractId
    depositData = Seq(master.toAddress.bytes.arr, swapContractId.bytes.arr, Longs.toByteArray(depositValue))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 3)
    // Register and deposit second token
    initToken2DataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, "init")
    regToken2Contract <- registerTokenGen(user, tContract, initToken2DataStack, description, fee + 10000000000L, ts + 4)
    token2ContractId = regToken2Contract.contractId
    issueToken2 <- issueTokenGen(user, token2ContractId, issueAmount, attach, fee, ts + 5)
    token2Id = tokenIdFromBytes(token2ContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    swap2DataStack <- initAtomicSwapContractDataStackGen(token2Id.arr)
    regSwap2Contract <- registerAtomicSwapGen(user, aContract, swap2DataStack, description, fee, ts + 6)
    swap2ContractId = regSwap2Contract.contractId
    deposit2Data = Seq(user.toAddress.bytes.arr, swap2ContractId.bytes.arr, Longs.toByteArray(depositValue))
    deposit2Type = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    deposit2Token <- depositTokenGen(user, token2ContractId, false, deposit2Data, deposit2Type, attach, fee, ts + 7)
    } yield (genesis, genesis2, master, user, regTokenContract, regToken2Contract, regSwapContract, regSwap2Contract, issueToken, issueToken2,
        depositToken, deposit2Token, ts, fee, description, attach)
}
