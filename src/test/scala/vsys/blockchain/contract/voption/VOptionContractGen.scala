package vsys.blockchain.contract.voption

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{Contract, ContractVOption, DataEntry, DataType}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VOptionContractGen {

  val supersedeIndex: Short = 0
  val activateIndex: Short = 1
  val mintIndex: Short = 2
  val unlockIndex: Short = 3
  val executeIndex: Short = 4
  val collectIndex: Short = 5

  def vOptionContractGen(): Gen[Contract] = ContractVOption.contract

  def genesisVOptionGen(rep: PrivateKeyAccount,
                     ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerVOptionGen(signer: PrivateKeyAccount,
                         contract: Contract,
                         dataStack: Seq[DataEntry],
                         description: String,
                         fee: Long,
                         ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(addr)

  def activateDataStackGen(maxIssueNum: Long,
                           price: Long,
                           priceUnit: Long): Gen[Seq[DataEntry]] = for {
    maxIssueNum <- Gen.const(DataEntry(Longs.toByteArray(maxIssueNum), DataType.Amount))
    price <- Gen.const(DataEntry(Longs.toByteArray(price), DataType.Amount))
    priceUnit <- Gen.const(DataEntry(Longs.toByteArray(priceUnit), DataType.Amount))
  } yield Seq(maxIssueNum, price, priceUnit)

  def commonOptionDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    amount <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(amount)

  def initVOptionDataStackGen(baseTokenId: Array[Byte],
                              targetTokenId: Array[Byte],
                              optionTokenId: Array[Byte],
                              proofTokenId: Array[Byte],
                              executeTime: Long,
                              executeDeadLine: Long): Gen[Seq[DataEntry]] = {
    for {
      baseTokenId <- Gen.const(DataEntry.create(baseTokenId, DataType.TokenId).right.get)
      targetTokenId <- Gen.const(DataEntry.create(targetTokenId, DataType.TokenId).right.get)
      optionTokenId <- Gen.const(DataEntry.create(optionTokenId, DataType.TokenId).right.get)
      proofTokenId <- Gen.const(DataEntry.create(proofTokenId, DataType.TokenId).right.get)
      executeTime <- Gen.const(DataEntry.create(Longs.toByteArray(executeTime), DataType.Timestamp).right.get)
      executeDeadLine <- Gen.const(DataEntry.create(Longs.toByteArray(executeDeadLine), DataType.Timestamp).right.get)
    } yield Seq(baseTokenId, targetTokenId, optionTokenId, proofTokenId, executeTime, executeDeadLine)
  }


  def supersedeVOptionGen(signer: PrivateKeyAccount,
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

  def activateVOptionGen(signer: PrivateKeyAccount,
                         contractId: ContractAccount,
                         maxIssueNum: Long,
                         price: Long,
                         priceUnit: Long,
                         attachment: Array[Byte],
                         fee: Long,
                         ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = activateIndex
    for {
      data <- activateDataStackGen(maxIssueNum, price, priceUnit)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def mintVOptionGen(signer: PrivateKeyAccount,
                     contractId: ContractAccount,
                     amount: Long,
                     attachment: Array[Byte],
                     fee: Long,
                     ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = mintIndex
    for {
      data <- commonOptionDataStackGen(amount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def unlockVOptionGen(signer: PrivateKeyAccount,
                       contractId: ContractAccount,
                       amount: Long,
                       attachment: Array[Byte],
                       fee: Long,
                       ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = unlockIndex
    for {
      data <- commonOptionDataStackGen(amount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def executeVOptionGen(signer: PrivateKeyAccount,
                        contractId: ContractAccount,
                        amount: Long,
                        attachment: Array[Byte],
                        fee: Long,
                        ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = executeIndex
    for {
      data <- commonOptionDataStackGen(amount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def collectVOptionGen(signer: PrivateKeyAccount,
                        contractId: ContractAccount,
                        amount: Long,
                        attachment: Array[Byte],
                        fee: Long,
                        ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = collectIndex
    for {
      data <- commonOptionDataStackGen(amount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

}
