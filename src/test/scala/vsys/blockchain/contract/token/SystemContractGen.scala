package vsys.blockchain.contract.token

import com.google.common.primitives.Longs
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{DataEntry, DataType, ContractGenHelper}
import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.state._
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction

trait SystemContractGen {

  val sysSend: Short = 0
  val sysDeposit: Short = 1
  val sysWithdraw: Short = 2
  val sysTransfer: Short = 3

  def sysSendDataStackGen(recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(reci, am)

  def sendVSYSGen(sender: PrivateKeyAccount, rep: Address, amount: Long, attachment: Array[Byte],
                  fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- sysSendDataStackGen(rep, amount)
    } yield ExecuteContractFunctionTransaction.create(sender, ContractAccount.systemContractId, sysSend, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def transferVSYSGen(signer: PrivateKeyAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, ContractAccount.systemContractId, sysTransfer, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def depositVSYSGen(signer: PrivateKeyAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal],
                     attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, ContractAccount.systemContractId, sysDeposit, data, attachment, fee, feeScale, ts).explicitGet()
  }

  def withdrawVSYSGen(signer: PrivateKeyAccount, data: Seq[Array[Byte]], dataType: Seq[DataType.DataTypeVal],
                      attachment: Array[Byte], fee: Long, ts: Long): Gen[ExecuteContractFunctionTransaction] = {
    for {
      data: Seq[DataEntry] <- ContractGenHelper.dataListGen(data, dataType)
    } yield ExecuteContractFunctionTransaction.create(signer, ContractAccount.systemContractId, sysWithdraw, data, attachment, fee, feeScale, ts).explicitGet()
  }

}
