package vsys.blockchain.contract.vescrow

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.ContractGenHelper.{ENOUGH_AMT, feeScale}
import vsys.blockchain.contract.{Contract, ContractVEscrow, DataEntry, DataType}
import vsys.blockchain.state._
import vsys.blockchain.transaction.GenesisTransaction
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

trait VEscrowContractGen {

  val supersedeIndex: Short = 0
  val createIndex: Short = 1
  val recipientDepositIndex: Short = 2
  val judgeDepositIndex: Short = 3
  val payerCancelIndex: Short = 4
  val recipientCancelIndex: Short = 5
  val judgeCancelIndex: Short = 6
  val submitWorkIndex: Short = 7
  val approveWorkIndex: Short = 8
  val applyToJudgeIndex: Short = 9
  val judgeIndex: Short = 10
  val submitPenaltyIndex: Short = 11
  val payerRefundIndex: Short = 12
  val recipientRefundIndex: Short = 13
  val collectIndex: Short = 14

  def vEscrowContractGen(): Gen[Contract] = ContractVEscrow.contract

  def genesisVEscrowGen(rep: PrivateKeyAccount,
                        ts: Long): Gen[GenesisTransaction] =
    GenesisTransaction.create(rep, ENOUGH_AMT, -1, ts).explicitGet()

  def registerVEscrowGen(signer: PrivateKeyAccount,
                         contract: Contract,
                         dataStack: Seq[DataEntry],
                         description: String,
                         fee: Long,
                         ts: Long): Gen[RegisterContractTransaction] =
    RegisterContractTransaction.create(signer, contract, dataStack, description, fee, feeScale, ts).explicitGet()

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    addr <- Gen.const(DataEntry.create(address.bytes.arr, DataType.Address).right.get)
  } yield Seq(addr)

  def createDataStackGen(recipient: Address,
                         amount: Long,
                         repDeposit: Long,
                         judgeDeposit: Long,
                         fee: Long,
                         refund: Long,
                         expirationTime: Long): Gen[Seq[DataEntry]] = for {
    recipient <- Gen.const(DataEntry.create(recipient.bytes.arr, DataType.Address).right.get)
    amount <- Gen.const(DataEntry.create(Longs.toByteArray(amount), DataType.Amount).right.get)
    repDeposit <- Gen.const(DataEntry.create(Longs.toByteArray(repDeposit), DataType.Amount).right.get)
    judgeDeposit <- Gen.const(DataEntry.create(Longs.toByteArray(judgeDeposit), DataType.Amount).right.get)
    fee <- Gen.const(DataEntry.create(Longs.toByteArray(fee), DataType.Amount).right.get)
    refund <- Gen.const(DataEntry.create(Longs.toByteArray(refund), DataType.Amount).right.get)
    expirationTime <- Gen.const(DataEntry.create(Longs.toByteArray(expirationTime), DataType.Amount).right.get)
  } yield Seq(recipient, amount, repDeposit, judgeDeposit, fee, refund, expirationTime)

  def escrowDepositDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def escrowCancelDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def submitWorkDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def approveWorkDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def applyToJudgeDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def judgeDataStackGen(orderId: Array[Byte],
                        payerAmount: Long,
                        recipientAmount: Long): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
    payerAmount <- Gen.const(DataEntry.create(Longs.toByteArray(payerAmount), DataType.Amount).right.get)
    recipientAmount <- Gen.const(DataEntry.create(Longs.toByteArray(recipientAmount), DataType.Amount).right.get)
  } yield Seq(orderId, payerAmount, recipientAmount)

  def submitPenaltyDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def payerRefundDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def recipientRefundDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def collectDataStackGen(orderId: Array[Byte]): Gen[Seq[DataEntry]] = for {
    orderId <- Gen.const(DataEntry.create(orderId, DataType.ShortBytes).right.get)
  } yield Seq(orderId)

  def initVEscrowDataStackGen(tokenId: Array[Byte], duration: Long, judgeDuration: Long): Gen[Seq[DataEntry]] = for {
    tokenId <- Gen.const(DataEntry.create(tokenId, DataType.TokenId).right.get)
    duration <- Gen.const(DataEntry.create(Longs.toByteArray(duration), DataType.Timestamp).right.get)
    judgeDuration <- Gen.const(DataEntry.create(Longs.toByteArray(judgeDuration), DataType.Timestamp).right.get)
  } yield Seq(tokenId, duration, judgeDuration)

  def supersedeVEscrowGen(signer: PrivateKeyAccount,
                          contractId: ContractAccount,
                          newJudge: Address,
                          attachment: Array[Byte],
                          fee: Long,
                          ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = supersedeIndex
    for {
      data: Seq[DataEntry] <- addressDataStackGen(newJudge)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def createVEscrowGen(signer: PrivateKeyAccount,
                       contractId: ContractAccount,
                       recipient: Address,
                       amount: Long,
                       repDeposit: Long,
                       judgeDeposit: Long,
                       judgeFee: Long,
                       refund: Long,
                       expirationTime: Long,
                       attachment: Array[Byte],
                       fee: Long,
                       ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = createIndex
    for {
      data: Seq[DataEntry] <- createDataStackGen(recipient, amount, repDeposit, judgeDeposit, judgeFee, refund, expirationTime)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def recipientDepositVEscrowGen(signer: PrivateKeyAccount,
                                 contractId: ContractAccount,
                                 orderId: Array[Byte],
                                 attachment: Array[Byte],
                                 fee: Long,
                                 ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = recipientDepositIndex
    for {
      data: Seq[DataEntry] <- escrowDepositDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def judgeDepositVEscrowGen(signer: PrivateKeyAccount,
                             contractId: ContractAccount,
                             orderId: Array[Byte],
                             attachment: Array[Byte],
                             fee: Long,
                             ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = judgeDepositIndex
    for {
      data: Seq[DataEntry] <- escrowDepositDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def payerCancelVEscrowGen(signer: PrivateKeyAccount,
                            contractId: ContractAccount,
                            orderId: Array[Byte],
                            attachment: Array[Byte],
                            fee: Long,
                            ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = payerCancelIndex
    for {
      data: Seq[DataEntry] <- escrowCancelDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def recipientCancelVEscrowGen(signer: PrivateKeyAccount,
                                contractId: ContractAccount,
                                orderId: Array[Byte],
                                attachment: Array[Byte],
                                fee: Long,
                                ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = recipientCancelIndex
    for {
      data: Seq[DataEntry] <- escrowCancelDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def judgeCancelVEscrowGen(signer: PrivateKeyAccount,
                            contractId: ContractAccount,
                            orderId: Array[Byte],
                            attachment: Array[Byte],
                            fee: Long,
                            ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = judgeCancelIndex
    for {
      data: Seq[DataEntry] <- escrowCancelDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def submitWorkVEscrowGen(signer: PrivateKeyAccount,
                           contractId: ContractAccount,
                           orderId: Array[Byte],
                           attachment: Array[Byte],
                           fee: Long,
                           ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = submitWorkIndex
    for {
      data: Seq[DataEntry] <- submitWorkDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def approveWorkVEscrowGen(signer: PrivateKeyAccount,
                            contractId: ContractAccount,
                            orderId: Array[Byte],
                            attachment: Array[Byte],
                            fee: Long,
                            ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = approveWorkIndex
    for {
      data: Seq[DataEntry] <- approveWorkDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def applyToJudgeVEscrowGen(signer: PrivateKeyAccount,
                             contractId: ContractAccount,
                             orderId: Array[Byte],
                             attachment: Array[Byte],
                             fee: Long,
                             ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = applyToJudgeIndex
    for {
      data: Seq[DataEntry] <- applyToJudgeDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def judgeVEscrowGen(signer: PrivateKeyAccount,
                      contractId: ContractAccount,
                      orderId: Array[Byte],
                      payerAmount: Long,
                      recipientAmount: Long,
                      attachment: Array[Byte],
                      fee: Long,
                      ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = judgeIndex
    for {
      data: Seq[DataEntry] <- judgeDataStackGen(orderId, payerAmount, recipientAmount)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def submitPenaltyVEscrowGen(signer: PrivateKeyAccount,
                              contractId: ContractAccount,
                              orderId: Array[Byte],
                              attachment: Array[Byte],
                              fee: Long,
                              ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = submitPenaltyIndex
    for {
      data: Seq[DataEntry] <- submitPenaltyDataStackGen(orderId: Array[Byte])
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def payerRefundVEscrowGen(signer: PrivateKeyAccount,
                            contractId: ContractAccount,
                            orderId: Array[Byte],
                            attachment: Array[Byte],
                            fee: Long,
                            ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = payerRefundIndex
    for {
      data: Seq[DataEntry] <- payerRefundDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def recipientRefundVEscrowGen(signer: PrivateKeyAccount,
                                contractId: ContractAccount,
                                orderId: Array[Byte],
                                attachment: Array[Byte],
                                fee: Long,
                                ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = recipientRefundIndex
    for {
      data: Seq[DataEntry] <- recipientRefundDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }

  def collectVEscrowGen(signer: PrivateKeyAccount,
                        contractId: ContractAccount,
                        orderId: Array[Byte],
                        attachment: Array[Byte],
                        fee: Long,
                        ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    val id: Short = collectIndex
    for {
      data: Seq[DataEntry] <- collectDataStackGen(orderId)
    } yield ExecuteContractFunctionTransaction.create(signer, contractId, id,
      data, attachment, fee, feeScale, ts).explicitGet()
  }
}