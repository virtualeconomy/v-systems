package vsys.utils

import vsys.blockchain.transaction._
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.blockchain.contract._
import vsys.blockchain.contract.{ContractPermitted, Contract}
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.ByteStr
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.Address

import scala.util.{Success, Failure}
import com.google.common.primitives.Ints

object TransactionHelper {

  def extractAmtFee(tx: ProcessedTransaction): (Long, Long) = {
    tx.transaction match {
      case tx: NonFeeTransaction with AmountInvolved => (tx.amount, 0)
      case txWithFee: AmountInvolved => (txWithFee.amount, txWithFee.transactionFee)
      case pTx: ProvenTransaction => (0, pTx.transactionFee)
      case _ => (0, 0)
    }
  }

  def execTxsFuncDataParser(txIn: ProcessedTransaction, state: StateReader): (ProcessedTransaction, Option[FuncDataStruct]) = {
  	val tx = txIn.transaction.asInstanceOf[ExecuteContractFunctionTransaction]
  	val funcInd = tx.funcIdx
    val txFee = tx.transactionFee
    val ctStr = tx.contractId.stringRepr
    val ctType = getContractType(ctStr, state)
    val signer = (tx.proofs.proofs(0).json \ "address").as[String]
    val data = tx.data
    val tokenIdStr: String = ByteStr.decodeBase58(ctStr) match {
      case Success(c) => tokenIdFromBytes(c.arr, Ints.toByteArray(0)) match {
        case Right(t : ByteStr) => t.base58
        case Left(e) => ""
      }
      case _ => ""
    }

    if (tokenIdStr == "") {
      return (txIn, None)
    }

    val funcData = (funcInd, ctType) match {
      case (0, _) =>
        Some(SupercedeFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[String]))
      case (1, _) =>
        Some(IssueFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[Long]))
      case (2, _) =>
        Some(DestroyFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[Long]))
      case (3, "TokenContractWithSplit") =>
        Some(SplitFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[Long]))
      case (3, "TokenContract") | (4, "TokenContractWithSplit") =>
        Some(SendFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[String], (data(1).json \ "data").as[Long]))
      case (4, "TokenContract") | (5, "TokenContractWithSplit") =>
        Some(TransferFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[String], (data(1).json \ "data").as[String], (data(2).json \ "data").as[Long]))
      case (5, "TokenContract") | (6, "TokenContractWithSplit") =>
        Some(DepositFuncData(funcInd, signer, ctStr, tokenIdStr, (data(0).json \ "data").as[String], (data(2).json \ "data").as[Long]))
      case (6, "TokenContract") | (7, "TokenContractWithSplit") =>
        Some(WithdrawFuncData(funcInd, signer, ctStr, tokenIdStr, (data(1).json \ "data").as[String], (data(2).json \ "data").as[Long]))
      case (_, _) => None
    }
    (txIn, funcData)
  }

  def getContractType(ctIdStr: String, state: StateReader): String = {
    ByteStr.decodeBase58(ctIdStr) match {
      case Success(id) => state.contractContent(id) match {
        case Some((h, txId, ct: Contract)) => ctTypeFromBytes(ct.bytes.arr)
        case None => "ContractNotExists"
        case _ => "InvalidContract"
      }
      case Failure(_) => "InvalidContract"
    }
  }

  def getTokenBalance(address: String, tokenIdStr: String, state: StateReader): Long = {
    Address.fromString(address) match {
      case Right(acc) =>
        ByteStr.decodeBase58(tokenIdStr) match {
          case Success(tokenId) => state.tokenAccountBalance(ByteStr(tokenId.arr ++ acc.bytes.arr))
          case Failure(_) => -1
        }
      case Left(_) => -1
    }
  }

  def getBalance(address: String, state: StateReader): Long = {
    Address.fromString(address) match {
      case Right(acc) => state.balance(acc)
      case Left(_) => -1
    }
  }

  private def ctTypeFromBytes(bytes: Array[Byte]): String = {
    if (bytes sameElements ContractPermitted.contract.bytes.arr) {
      "TokenContractWithSplit"
    } else if (bytes sameElements ContractPermitted.contractWithoutSplit.bytes.arr) {
      "TokenContract"
    } else {
      "GeneralContract"
    }
  }
}