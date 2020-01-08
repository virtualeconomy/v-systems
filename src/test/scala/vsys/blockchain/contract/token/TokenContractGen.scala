package vsys.blockchain.contract.token

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractPermitted, DataEntry, DataType, ContractGenHelper}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait TokenContractGen {

  val supersedeIndex: Short = 0
  val issueIndex: Short = 1
  val destroyIndex: Short = 2
  val splitIndex: Short = 3
  val sendIndex: Short = 4
  val transferIndex: Short = 5
  val depositIndex: Short = 6
  val withdrawIndex: Short = 7
  val totalSupplyIndex: Short = 8
  val maxSupplyIndex: Short = 9
  val balanceOfIndex: Short = 10
  val getIssuerIndex: Short = 11

  def initTokenDataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

  def amountDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    res <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(res)

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    add <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(add)

  def sendDataStackGen(recipient: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(reci, am, index)

  def sendDataStackGen(recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(reci, am)

  def tokenContractGen(split: Boolean): Gen[Contract] =
    if (split) ContractPermitted.contract
    else ContractPermitted.contractWithoutSplit

  def registerTokenGen(signer: PrivateKeyAccount, contract: Contract, dataStack: Seq[DataEntry],
                          description: String, fee: Long, ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def genesisTokenGen(rep: PrivateKeyAccount, ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def supersedeTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, newAdd: Address,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- addressDataStackGen(newAdd)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, supersedeIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def issueTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, amount: Long,
                    attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- amountDataStackGen(amount)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, issueIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def destroyTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, amount: Long,
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- amountDataStackGen(amount)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, destroyIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def splitTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, newUnity: Long,
                    attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = for {
    data: Seq[DataEntry] <- amountDataStackGen(newUnity)
  } yield ExecuteContractFunctionTransaction.create(signer, contractId, splitIndex, data, attachment, fee, feeScale, ts).explicitGet()

  def sendTokenGen(sender: PrivateKeyAccount, contractId: ContractAccount, split: Boolean, rep: Address, amount: Long,
                   attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) sendIndex else (sendIndex - 1).toShort
    for {
      data: Seq[DataEntry] <- sendDataStackGen(rep, amount)
    } yield ExecuteContractFunctionTransaction.create(sender, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def transferTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) transferIndex else (transferIndex - 1).toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def depositTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                       attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) depositIndex else (depositIndex - 1).toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def withdrawTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean, data: Seq[Array[Byte]], dataType: Seq[DataType.Value],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) withdrawIndex else (withdrawIndex - 1).toShort
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def totalSupplyTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean,
                          attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) totalSupplyIndex else (totalSupplyIndex - 1).toShort
    ExecuteContractFunctionTransaction.create(signer, contractId, id, Nil, attachment, fee, feeScale, ts).explicitGet()
  }

  def maxSupplyTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) maxSupplyIndex else (maxSupplyIndex - 1).toShort
    ExecuteContractFunctionTransaction.create(signer, contractId, id, Nil, attachment, fee, feeScale, ts).explicitGet()
  }

  def balanceOfTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean, add: Address,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) balanceOfIndex else (balanceOfIndex - 1).toShort
    for {
      data: Seq[DataEntry] <- addressDataStackGen(add)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def getIssuerTokenGen(signer: PrivateKeyAccount, contractId: ContractAccount, split: Boolean,
                        attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = if (split) getIssuerIndex else (getIssuerIndex - 1).toShort
    ExecuteContractFunctionTransaction.create(signer, contractId, id, Nil, attachment, fee, feeScale, ts).explicitGet()
  }
}
