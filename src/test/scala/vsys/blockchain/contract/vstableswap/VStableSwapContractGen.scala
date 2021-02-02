package vsys.blockchain.contract.vstableswap

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract._
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VStableSwapContractGen {

  val supersedeIndex: Short = 0
  val setOrderIndex: Short = 1
  val updateIndex: Short = 2
  val orderDepositIndex: Short = 3
  val orderWithdrawIndex: Short = 4
  val closeIndex: Short = 5
  val swapBaseToTargetIndex: Short = 6
  val swapTargetToBaseIndex: Short = 7

  def vStableSwapContractGen(): Gen[Contract] = ContractVStableSwap.contract

  def genesisVStableSwapGen(rep: PrivateKeyAccount,
                      ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerVStableSwapGen(signer: PrivateKeyAccount,
                             contract: Contract,
                             dataStack: Seq[DataEntry],
                             description: String,
                             fee: Long,
                             ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(addr)

  def setOrderDataStackGen(feeBase: Long,
                           feeTarget: Long,
                           minBase: Long,
                           maxBase: Long,
                           minTarget: Long,
                           maxTarget: Long,
                           priceBase: Long,
                           priceTarget: Long,
                           baseDeposit: Long,
                           targetDeposit: Long): Gen[Seq[DataEntry]] = for {
    feeBase <- Gen.const(DataEntry(Longs.toByteArray(feeBase), DataType.Amount))
    feeTarget <- Gen.const(DataEntry(Longs.toByteArray(feeTarget), DataType.Amount))
    minBase <- Gen.const(DataEntry(Longs.toByteArray(minBase), DataType.Amount))
    maxBase <- Gen.const(DataEntry(Longs.toByteArray(maxBase), DataType.Amount))
    minTarget <- Gen.const(DataEntry(Longs.toByteArray(minTarget), DataType.Amount))
    maxTarget <- Gen.const(DataEntry(Longs.toByteArray(maxTarget), DataType.Amount))
    priceBase <- Gen.const(DataEntry(Longs.toByteArray(priceBase), DataType.Amount))
    priceTarget <- Gen.const(DataEntry(Longs.toByteArray(priceTarget), DataType.Amount))
    baseDeposit <- Gen.const(DataEntry(Longs.toByteArray(baseDeposit), DataType.Amount))
    targetDeposit <- Gen.const(DataEntry(Longs.toByteArray(targetDeposit), DataType.Amount))
  } yield Seq(feeBase, feeTarget, minBase, maxBase, minTarget, maxTarget, priceBase, priceTarget, baseDeposit, targetDeposit)

  def updateDataStackGen(orderId: Array[Byte],
                         feeBase: Long,
                         feeTarget: Long,
                         minBase: Long,
                         minTarget: Long,
                         maxTarget: Long): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
    feeBase <- Gen.const(DataEntry(Longs.toByteArray(feeBase), DataType.Amount))
    feeTarget <- Gen.const(DataEntry(Longs.toByteArray(feeTarget), DataType.Amount))
    minBase <- Gen.const(DataEntry(Longs.toByteArray(minBase), DataType.Amount))
    minTarget <- Gen.const(DataEntry(Longs.toByteArray(minTarget), DataType.Amount))
    maxTarget <- Gen.const(DataEntry(Longs.toByteArray(maxTarget), DataType.Amount))
  } yield Seq(orderId, feeBase, feeTarget, minBase, minTarget, maxTarget)

  def orderDepositDataStackGen(orderId: Array[Byte],
                               baseDeposit: Long,
                               targetDeposit: Long): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
    baseDeposit <- Gen.const(DataEntry(Longs.toByteArray(baseDeposit), DataType.Amount))
    targetDeposit <- Gen.const(DataEntry(Longs.toByteArray(targetDeposit), DataType.Amount))
  } yield Seq(orderId, baseDeposit, targetDeposit)

  def orderWithdrawDataStackGen(orderId: Array[Byte],
                                baseWithdraw: Long,
                                targetWithdraw: Long): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
    baseWithdraw <- Gen.const(DataEntry(Longs.toByteArray(baseWithdraw), DataType.Amount))
    targetWithdraw <- Gen.const(DataEntry(Longs.toByteArray(targetWithdraw), DataType.Amount))
  } yield Seq(orderId, baseWithdraw, targetWithdraw)

  def closeDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
  } yield Seq(orderId)

  def swapBaseToTargetDataStackGen(orderId: Array[Byte],
                                   amount: Long,
                                   fee: Long,
                                   price: Long,
                                   deadLine: Long): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
    amount <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    fee <- Gen.const(DataEntry(Longs.toByteArray(fee), DataType.Amount))
    price <- Gen.const(DataEntry(Longs.toByteArray(price), DataType.Amount))
    deadLine <- Gen.const(DataEntry(Longs.toByteArray(deadLine), DataType.Timestamp))
  } yield Seq(orderId, amount, fee, price, deadLine)

  def swapTargetToBaseDataStackGen(orderId: Array[Byte],
                                   amount: Long,
                                   fee: Long,
                                   price: Long,
                                   deadLine: Long): Gen[Seq[DataEntry]] = for {
    orderId  <- Gen.const(DataEntry(orderId, DataType.ShortBytes))
    amount <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    fee <- Gen.const(DataEntry(Longs.toByteArray(fee), DataType.Amount))
    price <- Gen.const(DataEntry(Longs.toByteArray(price), DataType.Amount))
    deadLine <- Gen.const(DataEntry(Longs.toByteArray(deadLine), DataType.Timestamp))
  } yield Seq(orderId, amount, fee, price, deadLine)

  def initVStableSwapDataStackGen(baseTokenId: Array[Byte],
                                  targetTokenId: Array[Byte],
                                  maxOrderPerUser: Long,
                                  unitPriceBase: Long,
                                  unitPriceTarget: Long): Gen[Seq[DataEntry]] = for {
    baseTokenId <- Gen.const(DataEntry.create(baseTokenId, DataType.TokenId).right.get)
    targetTokenId <- Gen.const(DataEntry.create(targetTokenId, DataType.TokenId).right.get)
    maxOrderPerUser <- Gen.const(DataEntry.create(Longs.toByteArray(maxOrderPerUser), DataType.Amount).right.get)
    unitPriceBase <- Gen.const(DataEntry.create(Longs.toByteArray(unitPriceBase), DataType.Amount).right.get)
    unitPriceTarget <- Gen.const(DataEntry.create(Longs.toByteArray(unitPriceTarget), DataType.Amount).right.get)
  } yield Seq(baseTokenId, targetTokenId, maxOrderPerUser, unitPriceBase, unitPriceTarget)

  def supersedeVStableSwapGen(signer: PrivateKeyAccount, contractId: ContractAccount, newAdd: Address,
                              attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- addressDataStackGen(newAdd)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, supersedeIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def setOrderVStableSwapGen(sender: PrivateKeyAccount,
                             contractId: ContractAccount,
                             feeBase: Long,
                             feeTarget: Long,
                             minBase: Long,
                             maxBase: Long,
                             minTarget: Long,
                             maxTarget: Long,
                             priceBase: Long,
                             priceTarget: Long,
                             baseDeposit: Long,
                             targetDeposit: Long,
                             attachment: Array[Byte],
                             fee: Long,
                             ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = setOrderIndex
    for {
      data: Seq[DataEntry] <- setOrderDataStackGen(feeBase, feeTarget, minBase, maxBase, minTarget, maxTarget, priceBase, priceTarget, baseDeposit, targetDeposit)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def updateVStableSwapGen(sender: PrivateKeyAccount,
                           contractId: ContractAccount,
                           orderId: Array[Byte],
                           feeBase: Long,
                           feeTarget: Long,
                           minBase: Long,
                           minTarget: Long,
                           maxTarget: Long,
                           attachment: Array[Byte],
                           fee: Long,
                           ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = updateIndex
    for {
      data: Seq[DataEntry] <- updateDataStackGen(orderId, feeBase, feeTarget, minBase, minTarget, maxTarget)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def orderDepositVStableSwapGen(sender: PrivateKeyAccount,
                                 contractId: ContractAccount,
                                 orderId: Array[Byte],
                                 baseDeposit: Long,
                                 targetDeposit: Long,
                                 attachment: Array[Byte],
                                 fee: Long,
                                 ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = orderDepositIndex
    for {
      data: Seq[DataEntry] <- orderDepositDataStackGen(orderId, baseDeposit, targetDeposit)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee,  feeScale, ts).explicitGet()
  }

  def orderWithdrawVStableSwapGen(sender: PrivateKeyAccount,
                                  contractId: ContractAccount,
                                  orderId: Array[Byte],
                                  baseWithdraw: Long,
                                  targetWithdraw: Long,
                                  attachment: Array[Byte],
                                  fee: Long,
                                  ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = orderWithdrawIndex
    for {
      data: Seq[DataEntry] <- orderWithdrawDataStackGen(orderId, baseWithdraw, targetWithdraw)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def closeVStableSwapGen(sender: PrivateKeyAccount,
                          contractId: ContractAccount,
                          orderId: Array[Byte],
                          attachment: Array[Byte],
                          fee: Long,
                          ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = closeIndex
    for {
      data: Seq[DataEntry] <- closeDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapBaseToTargetVStableSwapGen(sender: PrivateKeyAccount,
                                     contractId: ContractAccount,
                                     orderId: Array[Byte],
                                     amount: Long,
                                     swapFee: Long,
                                     price: Long,
                                     deadLine: Long,
                                     attachment: Array[Byte],
                                     fee: Long,
                                     ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapBaseToTargetIndex
    for {
      data: Seq[DataEntry] <- swapBaseToTargetDataStackGen(orderId, amount, swapFee, price, deadLine)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def swapTargetToBaseVStableSwapGen(sender: PrivateKeyAccount,
                                     contractId: ContractAccount,
                                     orderId: Array[Byte],
                                     amount: Long,
                                     swapFee: Long,
                                     price: Long,
                                     deadLine: Long,
                                     attachment: Array[Byte],
                                     fee: Long,
                                     ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = swapTargetToBaseIndex
    for {
      data: Seq[DataEntry] <- swapTargetToBaseDataStackGen(orderId, amount, swapFee, price, deadLine)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }
}