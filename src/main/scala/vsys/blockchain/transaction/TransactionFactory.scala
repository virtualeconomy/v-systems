package vsys.blockchain.transaction

import scorex.crypto.encode.Base58
import vsys.account._
import vsys.api.http.contract.{ExecuteContractFunctionRequest, RegisterContractRequest}
import vsys.api.http.database.DbPutRequest
import vsys.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import vsys.api.http.payment.PaymentRequest
import vsys.api.http.spos.{ContendSlotsRequest, ReleaseSlotsRequest}
import vsys.blockchain.contract.{Contract, DataEntry}
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.utils.Time
import vsys.utils.serialization.Deser
import vsys.wallet.Wallet

object TransactionFactory {

  def createPayment(request: PaymentRequest, wallet: Wallet, time: Time): Either[ValidationError, PaymentTransaction] = for {
    publicKey <- wallet.findPrivateKey(request.sender)
    recipient <- Address.fromString(request.recipient)
    tx <- PaymentTransaction
      .create(
        publicKey,
        recipient,
        request.amount,
        request.fee,
        request.feeScale,
        time.getTimestamp(),
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray))
  } yield tx

  def lease(request: LeaseRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    recipientAcc <- Address.fromString(request.recipient)
    tx <- LeaseTransaction.create(senderPrivateKey, request.amount, request.fee, request.feeScale, time.getTimestamp(), recipientAcc)
  } yield tx

  def leaseCancel(request: LeaseCancelRequest, wallet: Wallet, time: Time): Either[ValidationError, LeaseCancelTransaction] =
    for {
      pk <- wallet.findPrivateKey(request.sender)
      tx <- LeaseCancelTransaction.create(pk, ByteStr.decodeBase58(request.txId).get, request.fee, request.feeScale, time.getTimestamp())
    } yield tx

  def contendSlots(request: ContendSlotsRequest, wallet:Wallet, time: Time): Either[ValidationError, ContendSlotsTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    tx <- ContendSlotsTransaction.create(senderPrivateKey, request.slotId, request.fee, request.feeScale, time.getTimestamp())
  } yield tx

  def releaseSlots(request: ReleaseSlotsRequest, wallet:Wallet, time: Time): Either[ValidationError, ReleaseSlotsTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    tx <- ReleaseSlotsTransaction.create(senderPrivateKey, request.slotId, request.fee, request.feeScale, time.getTimestamp())
  } yield tx
    
  def registerContract(request: RegisterContractRequest, wallet: Wallet, time: Time): Either[ValidationError, RegisterContractTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    contract <- Contract.fromBase58String(request.contract)
    initData <- DataEntry.fromBase58String(request.initData)
    tx <- RegisterContractTransaction
      .create(senderPrivateKey,
        contract,
        initData,
        request.description.filter(_.nonEmpty).getOrElse(Deser.deserilizeString(Array.emptyByteArray)),
        request.fee,
        request.feeScale,
        time.getTimestamp())
  } yield tx

  def executeContractFunction(request: ExecuteContractFunctionRequest, wallet: Wallet, time: Time): Either[ValidationError, ExecuteContractFunctionTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    contractId <- ContractAccount.fromString(request.contractId)
    functionData <- DataEntry.fromBase58String(request.functionData)
    tx <- ExecuteContractFunctionTransaction
      .create(senderPrivateKey,
        contractId,
        request.functionIndex,
        functionData,
        request.attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        request.fee,
        request.feeScale,
        time.getTimestamp())
  } yield tx

  def dbPut(request: DbPutRequest, wallet: Wallet, time: Time): Either[ValidationError, DbPutTransaction] = for {
    senderPrivateKey <- wallet.findPrivateKey(request.sender)
    tx <- DbPutTransaction.create(senderPrivateKey, request.dbKey, request.dataType, request.data, request.fee, request.feeScale, time.getTimestamp())
  } yield tx

}
