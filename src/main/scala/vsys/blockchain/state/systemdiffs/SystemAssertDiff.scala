package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.opcdiffs.OpcDiff
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData, ContractInvalidSigner}

import scala.util.{Left, Right}

object SystemAssertDiff {

  def isSignerOrigin(context: ExecutionContext)(address: DataEntry): Either[ValidationError, OpcDiff] = {
    val signer = context.signers.head
    if (address.dataType != DataType.Address)
      Left(ContractDataTypeMismatch)
    else if (!(address.data sameElements signer.bytes.arr))
      Left(ContractInvalidSigner)
    else
      Right(OpcDiff.empty)
  }

  object AssertType extends Enumeration {
    val IsSignerOriginAssert = Value(1)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == AssertType.IsSignerOriginAssert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => isSignerOrigin(context)(data(bytes(1)))
    case _ => Left(ContractInvalidOPCData)
  }
}
