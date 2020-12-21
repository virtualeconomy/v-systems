package vsys.blockchain.contract.vswap

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractVSwap, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}


trait VSwapContractGen {

  val supersedeIndex: Short = 0
  val setSwapIndex: Short = 1
  val addLiquidityIndex: Short = 2
  val removeLiquidityIndex: Short = 3
  val swapTokenForExactBaseTokenIndex: Short = 4
  val swapExactTokenForBaseTokenIndex: Short = 5
  val swapTokenForExactTargetTokenIndex: Short = 6
  val swapExactTokenForTargetTokenIndex: Short = 7

  def vSwapContractGen(): Gen[Contract] = ContractVSwap.contract

  def genesisVSwapGen(rep: PrivateKeyAccount,
                      ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerVSwapDataStackGen(tokenAId: Array[Byte],
                                tokenBId: Array[Byte],
                                liquidityTokenId: Array[Byte],
                                minimumLiquidity: Long): Gen[Seq[DataEntry]] = for {
    tokenAId <- Gen.const(DataEntry(tokenAId, DataType.TokenId))
    tokenBId <- Gen.const(DataEntry(tokenBId, DataType.TokenId))
    liquidityTokenId <- Gen.const(DataEntry(liquidityTokenId, DataType.TokenId))
    minimumLiquidity <- Gen.const(DataEntry(Longs.toByteArray(minimumLiquidity), DataType.Amount))
  } yield Seq(tokenAId, tokenBId, liquidityTokenId, minimumLiquidity)

  def registerVSwapGen(signer: PrivateKeyAccount,
                       contract: Contract,
                       dataStack: Seq[DataEntry],
                       description: String,
                       fee: Long,
                       ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(addr)

  def setSwapDataStackGen(amountADesired: Long,
                          amountBDesired: Long): Gen[Seq[DataEntry]] = for {
    amountADesired <- Gen.const(DataEntry(Longs.toByteArray(amountADesired), DataType.Amount))
    amountBDesired <- Gen.const(DataEntry(Longs.toByteArray(amountBDesired), DataType.Amount))
  } yield Seq(amountADesired, amountBDesired)

  def addLiquidityDataStackGen(amountADesired: Long,
                               amountBDesired: Long,
                               amountAMin: Long,
                               amountBMin: Long,
                               deadline: Long): Gen[Seq[DataEntry]] = for {
    amountADesired <- Gen.const(DataEntry(Longs.toByteArray(amountADesired), DataType.Amount))
    amountBDesired <- Gen.const(DataEntry(Longs.toByteArray(amountBDesired), DataType.Amount))
    amountAMin <- Gen.const(DataEntry(Longs.toByteArray(amountAMin), DataType.Amount))
    amountBMin <- Gen.const(DataEntry(Longs.toByteArray(amountBMin), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(amountADesired, amountBDesired, amountAMin, amountBMin, deadline)

  def removeLiquidityDataStackGen(liquidity: Long,
                                  amountAMin: Long,
                                  amountBMin: Long,
                                  deadline: Long): Gen[Seq[DataEntry]] = for {
    liquidity <- Gen.const(DataEntry(Longs.toByteArray(liquidity), DataType.Amount))
    amountAMin <- Gen.const(DataEntry(Longs.toByteArray(amountAMin), DataType.Amount))
    amountBMin <- Gen.const(DataEntry(Longs.toByteArray(amountBMin), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(liquidity, amountAMin, amountBMin, deadline)

  def swapTokenForExactBaseTokenDataStackGen(amountOut: Long,
                                             amountInMax: Long,
                                             deadline: Long): Gen[Seq[DataEntry]] = for {
    amountOut <- Gen.const(DataEntry(Longs.toByteArray(amountOut), DataType.Amount))
    amountInMax <- Gen.const(DataEntry(Longs.toByteArray(amountInMax), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(amountOut, amountInMax, deadline)

  def swapExactTokenForBaseTokenDataStackGen(amountOutMin: Long,
                                             amountIn: Long,
                                             deadline: Long): Gen[Seq[DataEntry]] = for {
    amountOutMin <- Gen.const(DataEntry(Longs.toByteArray(amountOutMin), DataType.Amount))
    amountIn <- Gen.const(DataEntry(Longs.toByteArray(amountIn), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(amountOutMin, amountIn, deadline)

  def swapTokenForExactTargetTokenDataStackGen(amountOut: Long,
                                               amountInMax: Long,
                                               deadline: Long): Gen[Seq[DataEntry]] = for {
    amountOut <- Gen.const(DataEntry(Longs.toByteArray(amountOut), DataType.Amount))
    amountInMax <- Gen.const(DataEntry(Longs.toByteArray(amountInMax), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(amountOut, amountInMax, deadline)

  def swapExactTokenForTargetTokenDataStackGen(amountOutMin: Long,
                                               amountIn: Long,
                                               deadline: Long): Gen[Seq[DataEntry]] = for {
    amountOutMin <- Gen.const(DataEntry(Longs.toByteArray(amountOutMin), DataType.Amount))
    amountIn <- Gen.const(DataEntry(Longs.toByteArray(amountIn), DataType.Amount))
    deadline <- Gen.const(DataEntry(Longs.toByteArray(deadline), DataType.Timestamp))
  } yield Seq(amountOutMin, amountIn, deadline)

  def initVSwapDataStackGen(tokenAId: Array[Byte],
                            tokenBId: Array[Byte],
                            liquidityTokenId: Array[Byte],
                            minimumLiquidity: Long): Gen[Seq[DataEntry]] = for {
    tokenAId <- Gen.const(DataEntry.create(tokenAId, DataType.TokenId).right.get)
    tokenBId <- Gen.const(DataEntry.create(tokenBId, DataType.TokenId).right.get)
    liquidityTokenId <- Gen.const(DataEntry.create(liquidityTokenId, DataType.TokenId).right.get)
    minimumLiquidity <- Gen.const(DataEntry.create(Longs.toByteArray(minimumLiquidity), DataType.Amount).right.get)
  } yield Seq(tokenAId, tokenBId, liquidityTokenId, minimumLiquidity)

  def supersedeVSwapGen(signer: PrivateKeyAccount,
                        contractId: ContractAccount,
                        newAddr: Address,
                        attachment: Array[Byte],
                        fee: Long,
                        ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = supersedeIndex
    for {
      data: Seq[DataEntry] <- addressDataStackGen(newAddr)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def setSwapVSwapGen(sender: PrivateKeyAccount,
                      contractId: ContractAccount,
                      amountADesired: Long,
                      amountBDesired: Long,
                      attachment: Array[Byte],
                      fee: Long,
                      ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = setSwapIndex
    for {
      data: Seq[DataEntry] <- setSwapDataStackGen(amountADesired, amountBDesired)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def addLiquidityVSwapGen(sender: PrivateKeyAccount,
                           contractId: ContractAccount,
                           amountADesired: Long,
                           amountBDesired: Long,
                           amountAMin: Long,
                           amountBMin: Long,
                           deadline: Long,
                           attachment: Array[Byte],
                           fee: Long,
                           ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = addLiquidityIndex
    for {
      data: Seq[DataEntry] <- addLiquidityDataStackGen(amountADesired, amountBDesired, amountAMin, amountBMin, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def removeLiquidityVSwapGen(sender: PrivateKeyAccount,
                              contractId: ContractAccount,
                              liquidity: Long,
                              amountAMin: Long,
                              amountBMin: Long,
                              deadline: Long,
                              attachment: Array[Byte],
                              fee: Long,
                              ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = removeLiquidityIndex
    for {
      data: Seq[DataEntry] <- removeLiquidityDataStackGen(liquidity, amountAMin, amountBMin, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapTokenForExactBaseTokenVSwapGen(sender: PrivateKeyAccount,
                                         contractId: ContractAccount,
                                         amountOut: Long,
                                         amountInMax: Long,
                                         deadline: Long,
                                         attachment: Array[Byte],
                                         fee: Long,
                                         ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapTokenForExactBaseTokenIndex
    for {
      data: Seq[DataEntry] <- swapTokenForExactBaseTokenDataStackGen(amountOut, amountInMax, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapExactTokenForBaseTokenVSwapGen(sender: PrivateKeyAccount,
                                         contractId: ContractAccount,
                                         amountOutMin: Long,
                                         amountIn: Long,
                                         deadline: Long,
                                         attachment: Array[Byte],
                                         fee: Long,
                                         ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapExactTokenForBaseTokenIndex
    for {
      data: Seq[DataEntry] <- swapExactTokenForBaseTokenDataStackGen(amountOutMin, amountIn, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapTokenForExactTargetTokenVSwapGen(sender: PrivateKeyAccount,
                                           contractId: ContractAccount,
                                           amountOut: Long,
                                           amountInMax: Long,
                                           deadline: Long,
                                           attachment: Array[Byte],
                                           fee: Long,
                                           ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapTokenForExactTargetTokenIndex
    for {
      data: Seq[DataEntry] <- swapTokenForExactTargetTokenDataStackGen(amountOut, amountInMax, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapExactTokenForTargetTokenVSwapGen(sender: PrivateKeyAccount,
                                           contractId: ContractAccount,
                                           amountOutMin: Long,
                                           amountIn: Long,
                                           deadline: Long,
                                           attachment: Array[Byte],
                                           fee: Long,
                                           ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapExactTokenForTargetTokenIndex
    for {
      data: Seq[DataEntry] <- swapExactTokenForTargetTokenDataStackGen(amountOutMin, amountIn, deadline)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }
}
