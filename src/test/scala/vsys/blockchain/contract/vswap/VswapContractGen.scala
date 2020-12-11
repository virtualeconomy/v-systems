package vsys.blockchain.contract.vswap

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract._
import vsys.blockchain.contract.lock.LockContractGen
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VswapContractGen extends TokenContractGen with LockContractGen {

  val supersededId: Short = 0
  val setSwapId: Short = 1
  val addLiquidityId: Short = 2
  val removeLiquidityId: Short = 3
  val swapTokenForExactBaseTokenId: Short = 4
  val swapExactTokenForBaseTokenId: Short = 5
  val swapTokenForExactTargetTokenId: Short = 6
  val swapExactTokenForTargetTokenId: Short = 7

  def vSwapContractGen(): Gen[Contract] =
    ContractVSwap.contract

  def genesisVSwapGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def initVSwapContractDataStackGen(tokenAId: Array[Byte], tokenBId: Array[Byte], liquidityTokenId: Array[Byte], minimumLiquidity: Long): Gen[Seq[DataEntry]] = for {
    tokenAId <- Gen.const(DataEntry(tokenAId, DataType.TokenId))
    tokenBId <- Gen.const(DataEntry(tokenBId, DataType.TokenId))
    liquidityTokenId <- Gen.const(DataEntry(liquidityTokenId, DataType.TokenId))
    minimumLiquidity <- Gen.const(DataEntry(Longs.toByteArray(minimumLiquidity), DataType.Amount))
  } yield Seq(tokenAId, tokenBId, liquidityTokenId, minimumLiquidity)

  def registerVSwapGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                                description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def setSwapGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = setSwapId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def addLiquidityGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = addLiquidityId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def removeLiquidityGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = removeLiquidityId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapTokenForExactBaseTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                 attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapTokenForExactBaseTokenId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapExactTokenForBaseTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                    attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapExactTokenForBaseTokenId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapTokenForExactTargetTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapTokenForExactTargetTokenId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapExactTokenForTargetTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal[_]],
                                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapExactTokenForTargetTokenId.toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  val tokenContract: Gen[Contract] = tokenContractGen(false)

  def createABLiquidityTokenAndInitVSwap(totalSupplyA: Long, unityA: Long, issueAmountA: Long, totalSupplyB: Long, unityB: Long, issueAmountB: Long,
                                         liquidityTotalSupply: Long, liquidityUnity: Long, liquidityIssueAmount: Long, minimumLiquidity: Long,
                                         tokenADepositAmount: Long, tokenBDepositAmount: Long): Gen[(GenesisTransaction, GenesisTransaction,
                                          PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction,
                                          RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
                                          ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
                                          ExecuteContractFunctionTransaction, Long, Long, Array[Byte])] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisVSwapGen(master, ts)
    tokenContract <- tokenContract
    user <- accountGen
    genesis2 <- genesisVSwapGen(user, ts)
    vSwapContract <- vSwapContractGen()
    // register token A
    initADataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyA, unityA, "init")
    description <- validDescStringGen
    regTokenAContract <- registerTokenGen(master, tokenContract, initADataStack, description, fee + 10000000000L, ts)
    tokenAContractId = regTokenAContract.contractId
    tokenAId = tokenIdFromBytes(tokenAContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register token B
    initBDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyB, unityB, "init")
    description <- validDescStringGen
    regTokenBContract <- registerTokenGen(master, tokenContract, initBDataStack, description, fee + 10000000000L, ts + 1)
    tokenBContractId = regTokenBContract.contractId
    tokenBId = tokenIdFromBytes(tokenBContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register liquidity token
    initLiquidityDataStack: Seq[DataEntry] <- initTokenDataStackGen(liquidityTotalSupply, liquidityUnity, "init")
    description <- validDescStringGen
    regLiquidityTokenContract <- registerTokenGen(master, tokenContract, initLiquidityDataStack, description, fee + 10000000000L, ts + 2)
    liquidityTokenContractId = regLiquidityTokenContract.contractId
    liquidityTokenId = tokenIdFromBytes(liquidityTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register VSwap contract
    initVSwapDataStack: Seq[DataEntry] <- initVSwapContractDataStackGen(tokenAId.arr, tokenBId.arr, liquidityTokenId.arr, minimumLiquidity)
    regVSwapContract <- registerVSwapGen(master, vSwapContract, initVSwapDataStack, description, fee + 10000000000L, ts + 3)
    vSwapContractId = regVSwapContract.contractId
    // issue token A
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueTokenA <- issueTokenGen(master, tokenAContractId, issueAmountA, attach, fee, ts + 4)
    // issue token B
    issueTokenB <- issueTokenGen(master, tokenBContractId, issueAmountB, attach, fee, ts + 5)
    // issue liquidity token
    issueLiquidityToken <- issueTokenGen(master, liquidityTokenContractId, liquidityIssueAmount, attach, fee, ts + 6)
    // deposit all issued tokens into swap contract
    depositTokenAData = Seq(master.toAddress.bytes.arr, vSwapContractId.bytes.arr, Longs.toByteArray(tokenADepositAmount))
    depositTokenAType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositTokenA <- depositTokenGen(master, tokenAContractId, false, depositTokenAData, depositTokenAType, attach, fee + 10000000000L, ts + 7)

    depositTokenBData = Seq(master.toAddress.bytes.arr, vSwapContractId.bytes.arr, Longs.toByteArray(tokenBDepositAmount))
    depositTokenBType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositTokenB <- depositTokenGen(master, tokenBContractId, false, depositTokenBData, depositTokenBType, attach, fee + 10000000000L, ts + 7)

    depositLiquidityData = Seq(master.toAddress.bytes.arr, vSwapContractId.bytes.arr, Longs.toByteArray(liquidityIssueAmount))
    depositLiquidityType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositLiquidity <- depositTokenGen(master, liquidityTokenContractId, false, depositLiquidityData, depositLiquidityType, attach, fee + 10000000000L, ts + 9)
  } yield (genesis, genesis2, master, user, regTokenAContract, regTokenBContract, regLiquidityTokenContract, regVSwapContract,
    issueTokenA, issueTokenB, issueLiquidityToken, depositTokenA, depositTokenB, depositLiquidity, fee, ts, attach)

}
