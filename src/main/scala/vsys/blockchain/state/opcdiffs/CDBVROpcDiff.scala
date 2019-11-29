package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.blockchain.state._
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.{ContractInvalidOPCData, ContractInvalidStateVariable, ContractLocalVariableIndexOutOfRange, ContractStateVariableNotDefined}
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.Contract.{checkStateMap, checkStateVar}

import scala.util.{Left, Right, Try}

object CDBVROpcDiff extends OpcDiffer {

  def get(context: ExecutionContext)(stateVar: Array[Byte], dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateVar(stateVar, DataType(stateVar(1)))) {
      Left(ContractInvalidStateVariable)
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVar(0)))) match {
        case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
        case _ => Left(ContractStateVariableNotDefined)
      }
    }
  }

  def mapGet(context: ExecutionContext)(stateMap: Array[Byte], keyValue: DataEntry, dataStack: Seq[DataEntry],
                                        pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateMap(stateMap, keyValue.dataType, DataType(stateMap(2)))) {
      Left(ValidationError.GenericError(s"Contract Invalid State Map"))
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val combinedKey = context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes
      if (DataType(stateMap(2)) == DataType.Amount) { // amount balance map
        val getVal = context.state.contractNumInfo(ByteStr(combinedKey))
        Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(getVal), DataType(stateMap(2)))), 1))
      } else {
        context.state.contractInfo(ByteStr(combinedKey)) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Left(ContractStateVariableNotDefined)
        }
      }
    }
  }

  def mapGetOrDefault(context: ExecutionContext)(stateMap: Array[Byte], keyValue: DataEntry, dataStack: Seq[DataEntry],
                                                 pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (!checkStateMap(stateMap, keyValue.dataType, DataType(stateMap(2)))) {
      Left(ValidationError.GenericError(s"Contract Invalid State Map"))
    } else if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      val combinedKey = context.contractId.bytes.arr ++ Array(stateMap(0)) ++ keyValue.bytes
      if (DataType(stateMap(2)) == DataType.Amount) { // amount balance map
        val getVal = context.state.contractNumInfo(ByteStr(combinedKey))
        Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(getVal), DataType.Amount)), 1))
      } else if (DataType(stateMap(2)) == DataType.Timestamp) { // timestamp
        context.state.contractInfo(ByteStr(combinedKey)) match {
          case Some(v) => Right(dataStack.patch(pointer, Seq(v), 1))
          case _ => Right(dataStack.patch(pointer, Seq(DataEntry(Longs.toByteArray(0L), DataType.Timestamp)), 1))
        }
      } else {
        Left(ContractStateVariableNotDefined)
      }
    }
  }

  object CDBVRType extends Enumeration(1) {
    val GetCDBVR, MapGetOrDefaultCDBVR = Value
  }

  private def checkCDBVRIndex(bytes: Array[Byte], id: Int, stateVarOrMapSize: Int): Boolean =
    bytes(id) < stateVarOrMapSize && bytes(id) >=0

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    (bytes.headOption.flatMap(f => Try(CDBVRType(f)).toOption), bytes.length) match {
      case (Some(CDBVRType.GetCDBVR), 3) if checkCDBVRIndex(bytes, 1, context.stateVar.length) =>
        get(context)(context.stateVar(bytes(1)), data, bytes(2))
      case (Some(CDBVRType.MapGetOrDefaultCDBVR), 4) if checkCDBVRIndex(bytes, 1, context.stateMap.length) && bytes(2) >=0 &&
        bytes(2) <= data.length => mapGetOrDefault(context)(context.stateMap(bytes(1)), data(bytes(2)), data, bytes(3))
      case _ => Left(ContractInvalidOPCData)
    }
}
