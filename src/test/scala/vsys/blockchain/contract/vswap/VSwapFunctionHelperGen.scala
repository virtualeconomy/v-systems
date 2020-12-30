package vsys.blockchain.contract.vswap

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VSwapFunctionHelperGen extends VSwapContractGen with TokenContractGen {

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

  def createABLiquidityTokenAndInitVSwap(totalSupplyA: Long,
                                         unityA: Long,
                                         issueAmountA: Long,
                                         totalSupplyB: Long,
                                         unityB: Long,
                                         issueAmountB: Long,
                                         liquidityTotalSupply: Long,
                                         liquidityUnity: Long,
                                         minimumLiquidity: Long,
                                         tokenADepositAmount: Long,
                                         tokenBDepositAmount: Long): Gen[(GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long, Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisVSwapGen(master, ts)
    user <- accountGen
    genesis2 <- genesisVSwapGen(user, ts)
    vSwapContract <- vSwapContractGen()
    // register token A
    regTokenAContract <- registerToken(master, totalSupplyA, unityA, "init", fee + 10000000000L, ts)
    tokenAContractId = regTokenAContract.contractId
    tokenAId = tokenIdFromBytes(tokenAContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register token B
    regTokenBContract <- registerToken(master, totalSupplyB, unityB, "init", fee + 10000000000L, ts + 1)
    tokenBContractId = regTokenBContract.contractId
    tokenBId = tokenIdFromBytes(tokenBContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register liquidity token
    regLiquidityTokenContract <- registerToken(master, liquidityTotalSupply, liquidityUnity, "init", fee + 10000000000L, ts + 2)
    liquidityTokenContractId = regLiquidityTokenContract.contractId
    liquidityTokenId = tokenIdFromBytes(liquidityTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register VSwap contract
    description <- validDescStringGen
    initVSwapDataStack: Seq[DataEntry] <- initVSwapDataStackGen(tokenAId.arr, tokenBId.arr, liquidityTokenId.arr, minimumLiquidity)
    regVSwapContract <- registerVSwapGen(master, vSwapContract, initVSwapDataStack, description, fee + 10000000000L, ts + 3)
    vSwapContractId = regVSwapContract.contractId
    // issue token A
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueTokenA <- issueToken(master, tokenAContractId, issueAmountA, fee, ts + 4)
    // issue token B
    issueTokenB <- issueToken(master, tokenBContractId, issueAmountB, fee, ts + 5)
    // issue liquidity token, always issue the entire supply of liquidity tokens
    issueLiquidityToken <- issueToken(master, liquidityTokenContractId, liquidityTotalSupply, fee, ts + 6)
    // deposit all issued tokens into swap contract, always deposit the entire supply of liquidity tokens
    depositTokenA <- depositToken(master, tokenAContractId, master.toAddress.bytes.arr, vSwapContractId.bytes.arr, tokenADepositAmount, fee + 10000000000L, ts + 7)
    depositTokenB <- depositToken(master, tokenBContractId, master.toAddress.bytes.arr, vSwapContractId.bytes.arr, tokenBDepositAmount, fee + 10000000000L, ts + 8)
    depositLiquidity <- depositToken(master, liquidityTokenContractId, master.toAddress.bytes.arr, vSwapContractId.bytes.arr, liquidityTotalSupply, fee + 10000000000L, ts + 9)
  } yield (genesis, genesis2, master, user, regTokenAContract, regTokenBContract, regLiquidityTokenContract, regVSwapContract,
    issueTokenA, issueTokenB, issueLiquidityToken, depositTokenA, depositTokenB, depositLiquidity, fee, ts, attach)
}
