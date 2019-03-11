package vsys.state.opcdiffs

import com.google.common.primitives.Shorts
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.serialization.Deser
import scorex.transaction.{Transaction, ValidationError}
import scorex.utils.ScorexLogging
import vsys.contract.{ContractContext, DataEntry}

import scala.util.Try


object OpcFuncDiffer extends ScorexLogging {

  def right(diff: OpcDiff): Either[ValidationError, OpcDiff] = Right(diff)

  def apply(contractContext: ContractContext)(data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    val opcFunc = contractContext.opcFunc
    val height = contractContext.height
    val tx = contractContext.transaction
    val s = contractContext.state
    val (_, _, listParaTypes, listOpcLines, _) = fromBytes(opcFunc).get
    if (listParaTypes.toSeq != data.map(_.dataType.id)) {
      Left(ValidationError.InvalidDataEntry)
    } else if (listOpcLines.forall(_.length >= 2)) {
      Left(ValidationError.InvalidContract)
    } else {
      listOpcLines.foldLeft(right(OpcDiff.empty)) { case (ei, opc) => ei.flatMap(opcDiff => {
        val blockDiff = opcDiff.asBlockDiff(height, tx)
        OpcDiffer(new CompositeStateReader(s, blockDiff), tx)(opc, data) match {
          case Right(newOpcDiff) => if (newOpcDiff == opcDiff) Right(newOpcDiff)
          else Left(TransactionValidationError(ValidationError.InvalidContract, tx))
          case Left(l) => Left(l)
        }
      })}
    }
    Right(OpcDiff.empty)

  }

  private def fromBytes(bytes: Array[Byte]): Try[(Short, Byte, Array[Byte], Seq[Array[Byte]], Seq[String])] = Try {
    val funcIdx = Shorts.fromByteArray(bytes.slice(0, 2))
    val (protoTypeBytes, protoTypeEnd) = Deser.parseArraySize(bytes, 2)
    val returnType = protoTypeBytes.head
    val listParaTypes = protoTypeBytes.tail
    val (listOpcLinesBytes, listOpcLinesEnd) = Deser.parseArraySize(bytes, protoTypeEnd)
    val listOpcLines = Deser.parseArrays(listOpcLinesBytes)
    val listVarNamesBytes = Deser.parseArrays(bytes.slice(listOpcLinesEnd, bytes.length))
    val listVarNames = listVarNamesBytes.map(x => Deser.deserilizeString(x))
    (funcIdx, returnType, listParaTypes, listOpcLines, listVarNames)
  }

}
