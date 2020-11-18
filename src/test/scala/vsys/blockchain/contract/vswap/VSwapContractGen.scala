package vsys.blockchain.contract.vswap

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractVSwap, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction


trait VSwapContractGen {

  val supersedeIndex: Short = 0
  val setSwapIndex: Short = 1
  val addLiquidityIndex: Short = 2
  val removeLiquidityIndex: Short = 3
  val swapTokenForExactBaseTokenIndex: Short = 4

  def vSwapContractGen(): Gen[Contract] = ContractVSwap.contract

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

  def initVSwapDataStackGen(tokenAId: Array[Byte],
                            tokenBId: Array[Byte],
                            liquidityTokenId: Array[Byte],
                            minimumLiquidity: Long): Gen[Seq[DataEntry]] = for {
    tokenAId <- Gen.const(DataEntry.create(tokenAId, DataType.TokenId).right.get)
    tokenBId <- Gen.const(DataEntry.create(tokenBId, DataType.TokenId).right.get)
    liquidityTokenId <- Gen.const(DataEntry.create(liquidityTokenId, DataType.TokenId).right.get)
    minimumLiquidity <- Gen.const(DataEntry.create(Longs.toByteArray(minimumLiquidity), DataType.TokenId).right.get)
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
}
