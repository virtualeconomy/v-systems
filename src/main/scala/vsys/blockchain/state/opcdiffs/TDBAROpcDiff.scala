package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidTokenIndex, ContractLocalVariableIndexOutOfRange}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.ExecutionContext

import scala.util.{Left, Right, Try}

object TDBAROpcDiff extends OpcDiffer {

  def balance(context: ExecutionContext)(address: DataEntry, tokenIndex: DataEntry,
                                         dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    if (tokenIndex.dataType != DataType.Int32 || address.dataType != DataType.Address) {
      Left(ContractDataTypeMismatch)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val contractTokens = context.state.contractTokens(context.contractId.bytes)
      val tokenNumber = Ints.fromByteArray(tokenIndex.data)
      val tokenID: ByteStr = tokenIdFromBytes(context.contractId.bytes.arr, tokenIndex.data).right.get
      val tokenBalanceKey = ByteStr(Bytes.concat(tokenID.arr, address.data))
      if (tokenNumber >= contractTokens || tokenNumber < 0) {
        Left(ContractInvalidTokenIndex)
      } else {
        val b = context.state.tokenAccountBalance(tokenBalanceKey)
        Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(b), DataType.Amount)), 1))
      }
    }
  }

  def balanceWithoutTokenIndex(context: ExecutionContext)(address: DataEntry,
                               dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {

    val tokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)
    balance(context)(address, tokenIndex, dataStack, pointer)
  }

  object TDBARType extends Enumeration {
    val BalanceTBDAR = Value(1)
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(TDBARType(f)).toOption) match {
      case Some(TDBARType.BalanceTBDAR) if checkInput(bytes.slice(0, bytes.length - 1), 2, context.stateVar.length, data.length, 1) =>
        balanceWithoutTokenIndex(context)(data(bytes(1)), data, bytes(2))
      case Some(TDBARType.BalanceTBDAR) if checkInput(bytes.slice(0, bytes.length - 1), 3, context.stateVar.length, data.length, 1) =>
        balance(context)(data(bytes(1)), data(bytes(2)), data, bytes(3))
      case _ => Left(ContractInvalidOPCData)
    }

  private def checkInput(bytes: Array[Byte], bLength: Int, stateVarLength: Int, dataLength: Int, sep: Int): Boolean = {
    bytes.length == bLength && bytes.slice(1, sep).forall(_ < stateVarLength) && bytes.slice(sep, bLength).forall(_ < dataLength) && bytes.tail.min >= 0
  }
}
