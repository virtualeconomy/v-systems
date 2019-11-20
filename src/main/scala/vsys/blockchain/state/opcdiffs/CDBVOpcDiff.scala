package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractInvalidStateVariable}
import vsys.account.Address
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.Contract.{checkStateMap, checkStateVar}

import scala.util.{Left, Right, Try}

object CDBVOpcDiff extends OpcDiffer {

  def set(context: ExecutionContext)(stateVar: Array[Byte],
                                     value: DataEntry): Either[ValidationError, OpcDiff] = {
    for {
      _ <- Either.cond(checkStateVar(stateVar, value.dataType), (), ContractInvalidStateVariable)
      addrOpt <- if (value.dataType == DataType.Address) Address.fromBytes(value.data).map(Some(_))
              else Right(None)
      contractDB = Map(ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0))) -> value.bytes)
      diff = addrOpt match {
        case Some(addr) => OpcDiff(relatedAddress = Map(addr -> true), contractDB = contractDB)
        case None => OpcDiff(contractDB = contractDB)
      }
    } yield diff
  }

  def mapSet(context: ExecutionContext)(stateMap: Array[Byte],
                                        keyValue: DataEntry, dataValue: DataEntry): Either[ValidationError, OpcDiff] = {
    for {
      // condition and  validation error need to be updated
      _ <- Either.cond(checkStateMap(stateMap, keyValue.dataType, dataValue.dataType), (), ContractInvalidStateVariable)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
      diff = if (dataValue.dataType == DataType.Amount || dataValue.dataType == DataType.Balance) OpcDiff(contractDB = Map(combinedKey -> dataValue.bytes))
             else OpcDiff(contractNumDB = Map(combinedKey -> Longs.fromByteArray(dataValue.data)))
    } yield diff
  }

  object CDBVType extends Enumeration {
    val SetCDBV = Value(1)
  }

  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    bytes.headOption.flatMap(f => Try(CDBVType(f)).toOption) match {
      case Some(CDBVType.SetCDBV) if bytes.length == 3 && bytes(1) < context.stateVar.length
        && bytes.last < data.length && bytes.tail.min >= 0 => set(context)(context.stateVar(bytes(1)), data(bytes(2)))
      case _ => Left(ContractInvalidOPCData)
    }
}
