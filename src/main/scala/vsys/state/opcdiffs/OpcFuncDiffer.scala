package vsys.state.opcdiffs

import com.google.common.primitives.Shorts
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.serialization.Deser
import scorex.transaction.{Transaction, ValidationError}
import scorex.utils.ScorexLogging
import vsys.contract.DataEntry

import scala.util.Try


object OpcFuncDiffer extends ScorexLogging {

  def right(diff: OpcDiff): Either[ValidationError, OpcDiff] = Right(diff)

  def apply(s: StateReader, height: Int, tx: Transaction)(opcfuncs: Seq[Array[Byte]],
                                                          dataStack: Seq[DataEntry]): Either[ValidationError,
    OpcDiff] = {
    val opcDiffEi = opcfuncs.foldLeft(right(OpcDiff.empty)) { case (ei, opcfunc) => ei.flatMap(opcDiff => {
      val blockDiff = opcDiff.asBlockDiff(height, tx)
      opcFuncDiffer(new CompositeStateReader(s, blockDiff), height, tx)(opcfunc, dataStack) match {
        case Right(newOpcDiff) => if (newOpcDiff == opcDiff) Right(newOpcDiff)
        else Left(TransactionValidationError(ValidationError.InvalidContract, tx))
        case Left(l) => Left(l)
      }
    })
    }
    opcDiffEi.map {d => d}
  }

  private def opcFuncDiffer(s: StateReader,
                            height: Int,
                            tx: Transaction)(opc: Array[Byte],
                                             dataStack: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    val (_, _, listParaTypes, listOpcodes, _) = fromBytes(opc).get
    if (listParaTypes.toSeq != dataStack.map(_.dataType.id)) {
      Left(ValidationError.InvalidDataEntry)
    } else if (listOpcodes.forall(_.length >= 2)) {
      Left(ValidationError.InvalidContract)
    } else {
      listOpcodes.foldLeft(right(OpcDiff.empty)) { case (ei, opcode) => ei.flatMap(opcDiff => {
        val blockDiff = opcDiff.asBlockDiff(height, tx)
        OpcDiffer(new CompositeStateReader(s, blockDiff))(opcode, dataStack) match {
          case Right(newOpcDiff) => if (newOpcDiff == opcDiff) Right(newOpcDiff)
          else Left(TransactionValidationError(ValidationError.InvalidContract, tx))
          case Left(l) => Left(l)
        }
      })}
    }
    Right(OpcDiff.empty)
  }

  private def fromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Seq[Array[Byte]], Seq[Short])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val (protoTypeBytes, protoTypeEnd) = Deser.parseArraySize(bytes, 2)
    val returnType = protoTypeBytes.head
    val listParaTypes = protoTypeBytes.tail
    val (listOpcodesBytes, listOpcodesEnd) = Deser.parseArraySize(bytes, protoTypeEnd)
    val listOpcodes = Deser.parseArrays(listOpcodesBytes)
    val listVarNames = Deser.deserializeShorts(bytes.slice(listOpcodesEnd, bytes.length))
    (funcIdx, returnType, listParaTypes, listOpcodes, listVarNames)
  }

}
