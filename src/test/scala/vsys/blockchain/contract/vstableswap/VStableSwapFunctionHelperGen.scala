package vsys.blockchain.contract.vstableswap

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.Gen
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.{Address, ContractAccount, PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VStableSwapFunctionHelperGen extends VStableSwapContractGen with TokenContractGen {

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

  def createBaseTokenTargetTokenAndInitVStableSwap(totalSupplyBase: Long,
                                                   unityBase: Long,
                                                   issueAmountBase: Long,
                                                   totalSupplyTarget: Long,
                                                   unityTarget: Long,
                                                   issueAmountTarget: Long,
                                                   maxOrderPerUser: Long,
                                                   unitPriceBase: Long,
                                                   unitPriceTarget: Long): Gen[(GenesisTransaction, GenesisTransaction,
    PrivateKeyAccount, PrivateKeyAccount, RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long, Array[Byte])] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisVStableSwapGen(master, ts)
    user <- accountGen
    genesis2 <- genesisVStableSwapGen(user, ts)
    vStableSwapContract <- vStableSwapContractGen()
    // Register base token
    regTokenBase <- registerToken(master, totalSupplyBase, unityBase, "init", fee, ts)
    tokenBaseContractId = regTokenBase.contractId
    tokenBaseId = tokenIdFromBytes(tokenBaseContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // Register target token
    regTokenTarget <- registerToken(master, totalSupplyTarget, unityTarget, "init", fee, ts + 1)
    tokenTargetContractId = regTokenTarget.contractId
    tokenTargetId = tokenIdFromBytes(tokenTargetContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // Register V Stable Swap Contract
    description <- validDescStringGen
    initVStableSwapDataStack <- initVStableSwapDataStackGen(tokenBaseId.arr, tokenTargetId.arr, maxOrderPerUser, unitPriceBase, unitPriceTarget)
    regVStableSwapContract <- registerVStableSwapGen(master, vStableSwapContract, initVStableSwapDataStack, description, fee, ts + 2)
    // Issue base token
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueTokenBase <- issueToken(master, tokenBaseContractId, issueAmountBase, fee, ts + 3)
    // Issue target token
    issueTokenTarget <- issueToken(master, tokenTargetContractId, issueAmountTarget, fee, ts + 4)
    // Deposit base and target token into V Stable Swap
    depositBase <- depositToken(master, tokenBaseContractId, master.toAddress.bytes.arr, regVStableSwapContract.contractId.bytes.arr, issueAmountBase, fee, ts + 5)
    depositTarget <- depositToken(master, tokenTargetContractId, master.toAddress.bytes.arr, regVStableSwapContract.contractId.bytes.arr, issueAmountTarget, fee, ts + 6)
  } yield (genesis, genesis2, master, user, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)

  def getContractTokenBalanceKeys(tokenBaseContractId: Array[Byte], tokenTargetContractId: Array[Byte], vStableSwapContractId: Array[Byte]): (ByteStr, ByteStr) = {
    val tokenBaseId = tokenIdFromBytes(tokenBaseContractId, Ints.toByteArray(0)).explicitGet()
    val tokenTargetId = tokenIdFromBytes(tokenTargetContractId, Ints.toByteArray(0)).explicitGet()

    val contractTokenBaseBalanceKey = ByteStr(Bytes.concat(tokenBaseId.arr, vStableSwapContractId))
    val contractTokenTargetBalanceKey = ByteStr(Bytes.concat(tokenTargetId.arr, vStableSwapContractId))

    (contractTokenBaseBalanceKey, contractTokenTargetBalanceKey)
  }

  def getUserTokenBalanceKeys(tokenBaseContractId: Array[Byte], tokenTargetContractId: Array[Byte], user: PublicKeyAccount): (ByteStr, ByteStr) = {
    val tokenBaseId = tokenIdFromBytes(tokenBaseContractId, Ints.toByteArray(0)).explicitGet()
    val tokenTargetId = tokenIdFromBytes(tokenTargetContractId, Ints.toByteArray(0)).explicitGet()

    val userTokenBaseBalanceKey = ByteStr(Bytes.concat(tokenBaseId.arr, user.toAddress.bytes.arr))
    val userTokenTargetBalanceKey = ByteStr(Bytes.concat(tokenTargetId.arr, user.toAddress.bytes.arr))

    (userTokenBaseBalanceKey, userTokenTargetBalanceKey)
  }

  def getStableSwapContractStateVarKeys(vStableSwapContractId: Array[Byte]): (ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr) = {
    val makerKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(0.toByte)))
    val baseTokenIdKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(1.toByte)))
    val targetTokenIdKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(2.toByte)))
    val maxOrderPerUserKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(3.toByte)))
    val unitPriceBaseKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(4.toByte)))
    val unitPriceTargetKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(5.toByte)))

    (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey)
  }

  def getStableSwapContractStateMapKeys(vStableSwapContractId: Array[Byte], orderId: Array[Byte], user: PublicKeyAccount): (ByteStr, ByteStr, ByteStr,
    ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr) = {
    val baseTokenBalanceKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(0.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))
    val targetTokenBalanceKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(1.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))
    val userOrdersKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(2.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))
    val orderOwnerKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(3.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val feeBaseKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(4.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val feeTargetKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(5.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val minBaseKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(6.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val maxBaseKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(7.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val minTargetKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(8.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val maxTargetKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(9.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val priceBaseKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(10.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val priceTargetKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(11.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val baseTokenLockedKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(12.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val targetTokenLockedKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(13.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))
    val orderStatusKey = ByteStr(Bytes.concat(vStableSwapContractId, Array(14.toByte), DataEntry.create(orderId, DataType.ShortBytes).right.get.bytes))

    (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
      minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey)
  }
}
