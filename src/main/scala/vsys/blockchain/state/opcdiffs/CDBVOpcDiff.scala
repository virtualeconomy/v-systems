package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
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
      _ <- Either.cond(checkStateMap(stateMap, keyValue.dataType, dataValue.dataType), (), ContractInvalidStateMap)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
      diff = OpcDiff(contractDB = Map(combinedKey -> dataValue.bytes))
    } yield diff
  }

  def mapValueAdd(context: ExecutionContext)(stateMap: Array[Byte],
                                             keyValue: DataEntry, dataValue: DataEntry): Either[ValidationError, OpcDiff] = {
    val combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
    if (!checkStateMap(stateMap, keyValue.dataType, dataValue.dataType))
      Left(ContractInvalidStateMap)
    if (dataValue.dataType == DataType.Amount){
      val cntBalance = context.state.contractNumInfo(combinedKey)
      val addAmount = Longs.fromByteArray(dataValue.data)
      if (Try(Math.addExact(cntBalance, addAmount)).isFailure)
        Left(ValidationError.OverflowError)
      else
        Right(OpcDiff(contractNumDB = Map(combinedKey -> addAmount)))
    }
    else Left(ContractDataTypeMismatch)
  }

  def mapValueMinus(context: ExecutionContext)(stateMap: Array[Byte],
                                               keyValue: DataEntry, dataValue: DataEntry): Either[ValidationError, OpcDiff] = {
    val combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
    if (!checkStateMap(stateMap, keyValue.dataType, dataValue.dataType))
      Left(ContractInvalidStateMap)
    if (dataValue.dataType == DataType.Amount){
      val cntBalance = context.state.contractNumInfo(combinedKey)
      val minusAmount = Longs.fromByteArray(dataValue.data)
      if (cntBalance >= minusAmount)
        Right(OpcDiff(contractNumDB = Map(combinedKey -> -minusAmount)))
      else Left(ContractMapValueInsufficient)
    }
    else Left(ContractDataTypeMismatch)
  }

  object CDBVType extends Enumeration(1) {
    val SetCDBV, mapSetCDBV, mapValueAddCDBV, mapValueMinusCDBV = Value
  }

  private def checkCDBVBytes(bytes: Array[Byte], dLength: Int, stateVarOrMapSize: Int): Boolean =
    (bytes.length == 2 || bytes.tail.tail.max < dLength) && bytes(1) < stateVarOrMapSize && bytes.tail.min >= 0

  override def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    (bytes.headOption.flatMap(f => Try(CDBVType(f)).toOption), bytes.length) match {
      case (Some(CDBVType.SetCDBV), 3) if checkCDBVBytes(bytes, data.length, context.stateVar.length) =>
        set(context)(context.stateVar(bytes(1)), data(bytes(2)))
      case (Some(CDBVType.mapSetCDBV), 4) if checkCDBVBytes(bytes, data.length, context.stateMap.length) =>
        mapSet(context)(context.stateMap(bytes(1)), data(bytes(2)), data(bytes(3)))
      case (Some(CDBVType.mapValueAddCDBV), 4) if checkCDBVBytes(bytes, data.length, context.stateMap.length) =>
        mapValueAdd(context)(context.stateMap(bytes(1)), data(bytes(2)), data(bytes(3)))
      case (Some(CDBVType.mapValueMinusCDBV), 4) if checkCDBVBytes(bytes, data.length, context.stateMap.length) =>
        mapValueMinus(context)(context.stateMap(bytes(1)), data(bytes(2)), data(bytes(3)))
      case _ => Left(ContractInvalidOPCData)
    }
}
