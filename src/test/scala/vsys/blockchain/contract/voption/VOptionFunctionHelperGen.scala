package vsys.blockchain.contract.voption

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VOptionFunctionHelperGen extends VOptionContractGen with TokenContractGen {

  override val supersedeIndex: Short = 0

  override def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(addr)

  def registerToken(user: PrivateKeyAccount,
                    totalSupply: Long,
                    unity: Long,
                    desc: String,
                    fee: Long,
                    timestamp: Long): Gen[RegisterContractTransaction] = for {
    initTokenDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupply, unity, desc)
    description <- validDescStringGen
    tokenContract <- tokenContractGen(false)
    regTokenContract <- registerTokenGen(user, tokenContract, initTokenDataStack, description, fee, timestamp)
  } yield regTokenContract

  def issueToken(user: PrivateKeyAccount,
                 contractId: ContractAccount,
                 issueAmount: Long,
                 fee: Long,
                 timestamp: Long): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(user, contractId, issueAmount, attach, fee, timestamp)
  } yield issueToken

  def depositToken(user: PrivateKeyAccount,
                   contractId: ContractAccount,
                   sender: Array[Byte],
                   contract: Array[Byte],
                   amount: Long,
                   fee: Long,
                   timestamp: Long
                  ): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositTokenData = Seq(sender, contract, Longs.toByteArray(amount))
    depositTokenDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(user, contractId, false, depositTokenData, depositTokenDataType, attach, fee, timestamp)
  } yield depositToken

  def withdrawToken(user: PrivateKeyAccount,
                    contractId: ContractAccount,
                    contract: Array[Byte],
                    sender: Array[Byte],
                    amount: Long,
                    fee: Long,
                    timestamp: Long): Gen[ExecuteContractFunctionTransaction] = for {
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    withdrawTokenData = Seq(contract, sender, Longs.toByteArray(amount))
    withdrawTokenDataType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawToken <- withdrawTokenGen(user, contractId, false, withdrawTokenData, withdrawTokenDataType, attach, fee, timestamp)
  } yield withdrawToken

  def createBaseTargetOptionProofTokenAndInitVOption(baseTotalSupply: Long, baseUnity: Long, baseIssueAmount: Long, targetTotalSupply: Long, targetUnity: Long,
                                                     targetIssueAmount: Long, optionTotalSupply: Long, optionUnity: Long, proofTotalSupply: Long,
                                           proofUnity: Long, baseTokenDepositAmount: Long, targetTokenDepositAmount: Long): Gen[(GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long, Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()

    genesis <- genesisVOptionGen(master, ts)
    user <- accountGen
    genesis2 <- genesisVOptionGen(user, ts)
    vOptionContract <- vOptionContractGen()

    // register base token
    regBaseTokenContract <- registerToken(master, baseTotalSupply, baseUnity, "init", fee + 10000000000L, ts)
    baseTokenContractId = regBaseTokenContract.contractId
    baseTokenId = tokenIdFromBytes(baseTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register target token
    regTargetTokenContract <- registerToken(master, targetTotalSupply, targetUnity, "init", fee + 10000000000L, ts + 1)
    targetTokenContractId = regTargetTokenContract.contractId
    targetTokenId = tokenIdFromBytes(targetTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register option token
    regOptionTokenContract <- registerToken(master, optionTotalSupply, optionUnity, "init", fee + 10000000000L, ts + 2)
    optionTokenContractId = regOptionTokenContract.contractId
    optionTokenId = tokenIdFromBytes(optionTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register proof token
    regProofTokenContract <- registerToken(master, proofTotalSupply, proofUnity, "init", fee + 10000000000L, ts + 3)
    proofTokenContractId = regProofTokenContract.contractId
    proofTokenId = tokenIdFromBytes(proofTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    // register VSwap contract
    description <- validDescStringGen
    initVOptionDataStack: Seq[DataEntry] <- initVOptionDataStackGen(baseTokenId.arr, targetTokenId.arr, optionTokenId.arr, proofTokenId.arr, ts + 100, ts + 200)
    regVOptionContract <- registerVOptionGen(master, vOptionContract, initVOptionDataStack, description, fee + 10000000000L, ts + 4)
    vOptionContractId = regVOptionContract.contractId

    // issue base token
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueBaseToken <- issueToken(master, baseTokenContractId, baseIssueAmount, fee, ts + 5)
    // issue target token
    issueTargetToken <- issueToken(master, targetTokenContractId, targetIssueAmount, fee, ts + 6)
    // issue option token, always issue the entire supply of option tokens
    issueOptionToken <- issueToken(master, optionTokenContractId, optionTotalSupply, fee, ts + 7)
    // issue proof token, always issue the entire supply of proof tokens
    issueProofToken <- issueToken(master, proofTokenContractId, proofTotalSupply, fee, ts + 8)

    // deposit all issued tokens into voption contract, always deposit the entire supply of option and proof tokens
    depositBaseToken <- depositToken(master, baseTokenContractId, master.toAddress.bytes.arr, vOptionContractId.bytes.arr, baseTokenDepositAmount, fee + 10000000000L, ts + 9)
    depositTargetToken <- depositToken(master, targetTokenContractId, master.toAddress.bytes.arr, vOptionContractId.bytes.arr, targetTokenDepositAmount, fee + 10000000000L, ts + 10)
    depositOptionToken <- depositToken(master, optionTokenContractId, master.toAddress.bytes.arr, vOptionContractId.bytes.arr, optionTotalSupply, fee + 10000000000L, ts + 11)
    depositProofToken <- depositToken(master, proofTokenContractId, master.toAddress.bytes.arr, vOptionContractId.bytes.arr, proofTotalSupply, fee + 10000000000L, ts + 12)
  } yield (genesis, genesis2, master, user, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken , depositProofToken, fee, ts, attach)
}
