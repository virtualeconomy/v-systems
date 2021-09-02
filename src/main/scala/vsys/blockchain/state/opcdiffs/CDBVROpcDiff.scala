package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.Contract.{checkStateMap, checkStateVar}

import scala.util.{Left, Right, Try}

object CDBVROpcDiff extends OpcDiffer {

  private def dbGet(context: ExecutionContext)(dbKey: ByteStr, dataTypeByte: Byte): Either[ValidationError, DataEntry] = {
    context.state.contractInfo(dbKey) match {
      case Some(v) => Right(v)
      case _ => Left(GenericError("Can not find the data."))
    }
  }

  private def dbGetOrDefault(context: ExecutionContext)(dbKey: ByteStr, dataTypeByte: Byte): Either[ValidationError, DataEntry] = {
    context.state.contractInfo(dbKey) match {
      case Some(v) => Right(v)
      case _ if DataType(dataTypeByte) == DataType.Timestamp =>
        Right(DataEntry(Longs.toByteArray(0L), DataType.Timestamp))
      case _ if DataType(dataTypeByte) == DataType.Amount =>
        Right(DataEntry(Longs.toByteArray(context.state.contractNumInfo(dbKey)), DataType.Amount))
      case _ if DataType(dataTypeByte) == DataType.Boolean =>
        Right(DataEntry(Array(0.toByte), DataType.Boolean))
      case _ => Left(GenericError("Can not find the data."))
    }
  }

  def get(context: ExecutionContext)(stateVar: Array[Byte], dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    for {
      _ <- Either.cond(checkStateVar(stateVar), (), ContractInvalidStateVariable)
      _ <- Either.cond(pointer <= dataStack.length && pointer >= 0, (), ContractLocalVariableIndexOutOfRange)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))
      updatedDataEntry <- dbGet(context)(combinedKey, stateVar(1)).left.map(_ => ContractStateVariableNotDefined)
      updatedSeq = dataStack.patch(pointer, Seq(updatedDataEntry), 1)
    } yield updatedSeq
  }

  def mapGet(context: ExecutionContext)(stateMap: Array[Byte], keyValue: DataEntry, dataStack: Seq[DataEntry],
                                        pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    for {
      _ <- Either.cond(checkStateMap(stateMap, keyValue.dataType), (), ContractInvalidStateMap)
      _ <- Either.cond(pointer <= dataStack.length && pointer >= 0, (), ContractLocalVariableIndexOutOfRange)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
      updatedDataEntry <- dbGet(context)(combinedKey, stateMap(2)).left.map(_ => ContractStateMapNotDefined)
      updatedSeq = dataStack.patch(pointer, Seq(updatedDataEntry), 1)
    } yield updatedSeq
  }

  def mapGetOrDefault(context: ExecutionContext)(stateMap: Array[Byte], keyValue: DataEntry, dataStack: Seq[DataEntry],
                                                 pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    for {
      _ <- Either.cond(checkStateMap(stateMap, keyValue.dataType), (), ContractInvalidStateMap)
      _ <- Either.cond(pointer <= dataStack.length && pointer >= 0, (), ContractLocalVariableIndexOutOfRange)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes)
      updatedDataEntry <- dbGetOrDefault(context)(combinedKey, stateMap(2)).left.map(_ => ContractStateMapNotDefined)
      updatedSeq = dataStack.patch(pointer, Seq(updatedDataEntry), 1)
    } yield updatedSeq
  }

  def getOrDefault(context: ExecutionContext)(stateVar: Array[Byte], dataStack: Seq[DataEntry],
                                              pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    for {
      _ <- Either.cond(checkStateVar(stateVar), (), ContractInvalidStateVariable)
      _ <- Either.cond(pointer <= dataStack.length && pointer >= 0, (), ContractLocalVariableIndexOutOfRange)
      combinedKey = ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))
      updatedDataEntry <- dbGetOrDefault(context)(combinedKey, stateVar(1)).left.map(_ => ContractStateVariableNotDefined)
      updatedSeq = dataStack.patch(pointer, Seq(updatedDataEntry), 1)
    } yield updatedSeq
  }

  object CDBVRType extends Enumeration(1) {
    val GetCDBVR, MapGetOrDefaultCDBVR, MapGetCDVVR, StateVarGetOrDefaultCDBVR = Value
  }

  private def checkCDBVRIndex(bytes: Array[Byte], id: Int, stateVarOrMapSize: Int): Boolean =
    bytes(id) < stateVarOrMapSize && bytes(id) >=0

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    (bytes.headOption.flatMap(f => Try(CDBVRType(f)).toOption), bytes.length) match {
      case (Some(CDBVRType.GetCDBVR), 3) if checkCDBVRIndex(bytes, 1, context.stateVar.length) =>
        get(context)(context.stateVar(bytes(1)), data, bytes(2))
      case (Some(CDBVRType.MapGetOrDefaultCDBVR), 4) if checkCDBVRIndex(bytes, 1, context.stateMap.length) && bytes(2) >=0 &&
        bytes(2) <= data.length => mapGetOrDefault(context)(context.stateMap(bytes(1)), data(bytes(2)), data, bytes(3))
      case (Some(CDBVRType.MapGetCDVVR), 4) if checkCDBVRIndex(bytes, 1, context.stateMap.length) && bytes(2) >=0 &&
        bytes(2) <= data.length => mapGet(context)(context.stateMap(bytes(1)), data(bytes(2)), data, bytes(3))
      case (Some(CDBVRType.StateVarGetOrDefaultCDBVR), 3) if checkCDBVRIndex(bytes, 1, context.stateVar.length) =>
        getOrDefault(context)(context.stateVar(bytes(1)), data, bytes(2))
      case _ => Left(ContractInvalidOPCData)
    }
}
