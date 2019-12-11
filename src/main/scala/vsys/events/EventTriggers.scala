package vsys.events

import vsys.utils.{ScorexLogging, TransactionHelper}
import vsys.settings.{EventSettings, BlockAppendedEventSettings, TxConfirmedEventSettings}
import vsys.settings.{BlockRollbackEventSettings, StateUpdatedEventSettings, WebhookEventRules}
import vsys.blockchain.transaction.{ProcessedTransaction, PaymentTransaction, MintingTransaction, AmountInvolved}
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.state.BlockDiff
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.block.Block
import vsys.blockchain.contract.{FuncDataStruct, SendFuncData, IssueFuncData, DestroyFuncData, FuncAmtInvolved}
import vsys.account.Address
import vsys.settings.{RelatedAccs, WithTxsOfTypes, WithTxsOfAccs, WithStateOfAccs}

import akka.actor.ActorRef
import play.api.libs.json.{JsObject, JsValue, Json, JsArray}
import java.util.UUID.randomUUID


class EventTriggers(eventWriter: ActorRef, eventSetting: EventSettings) extends ScorexLogging {

  // TO DO: Should handle more webhook event settings
  def evokeWebhook(block: Block, blockDiff: BlockDiff, state: StateReader, evokeFrom: String): Unit = {
    val webhookSettings = eventSetting.webhookSettings
    webhookSettings.map {webhookSetting =>
      val url = webhookSetting.url
      val scKey = webhookSetting.secretKey
      val enKey = webhookSetting.encryptKey
      val maxSize = eventSetting.maxSize
      val blockJson = block.json

      webhookSetting.events.map {webhookEventSetting =>
        webhookEventSetting match {
          case e: BlockAppendedEventSettings if (evokeFrom == "processBlock")=>
            val height = getHeight(blockDiff)
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff)
            if (re.size > 0) {
              val txJson = formTxJson(re)
              val rulesJson = formRuleJson(e.eventRules)
              val uuid = randomUUID
              val payload: JsValue = Json.obj(
                  "timestamp" -> blockJson("timestamp"),
                  "reference" -> blockJson("reference"),
                  "SposConsensus" -> Json.obj(
                    "mintTime" -> block.consensusData.mintTime,
                    "mintBalance" -> block.consensusData.mintBalance
                  ),
                  "merkleRoot" -> blockJson("TransactionMerkleRoot"),
                  "generator" -> blockJson("generator"),
                  "signature" -> blockJson("signature"),
                  "blocksize" -> blockJson("blocksize"),
                  "height" -> height,
                  "transactionCount" -> re.size,
                  "transactions" -> txJson
              )
              val subscribeData: JsObject = Json.obj(
                "type" -> 1,
                "uuid" -> uuid,
                "payload" -> payload,
                "referringRules" -> rulesJson
              )

              eventWriter ! BlockAppendedEvent(url, scKey, enKey, maxSize, subscribeData)
            }

          case e: TxConfirmedEventSettings if (evokeFrom == "processBlock") =>
            val height = getHeight(blockDiff)
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff)
            val txJson = formTxJson(re)
            val rulesJson = formRuleJson(e.eventRules)
            val uuid = randomUUID

            if (txJson.size > 0){
              val payload: JsValue = Json.obj(
                "transactionCount" -> txJson.size,
                "transactions" -> txJson
              )
              val subscribeData: JsObject = Json.obj(
                "type" -> 2,
                "uuid" -> uuid,
                "payload" -> payload,
                "referringRules" -> rulesJson
              )
              eventWriter ! TxConfirmedEvent(url, scKey, enKey, maxSize, subscribeData)
            }

          case e: StateUpdatedEventSettings if (evokeFrom == "processBlock") =>
            val height = getHeight(blockDiff)
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff)
            val contractTxs = extractTxs(re, 9)
            val normalTxs = Seq(2, 5).map(num => (num, extractTxs(re, num)))
            val rulesJson = formRuleJson(e.eventRules)
            val blockTxsJson = formTxJson(re)
            val blockResultJson: JsObject = Json.obj(
                "timestamp" -> blockJson("timestamp"),
                "reference" -> blockJson("reference"),
                "SposConsensus" -> Json.obj(
                  "mintTime" -> block.consensusData.mintTime,
                  "mintBalance" -> block.consensusData.mintBalance
                ),
              "merkleRoot" -> blockJson("TransactionMerkleRoot"),
              "generator" -> blockJson("generator"),
              "signature" -> blockJson("signature"),
              "blocksize" -> blockJson("blocksize"),
              "height" -> height,
              "transactionCount" -> re.size,
              "transactions" -> blockTxsJson
            )

            // check for contract txs
            if (contractTxs.size > 0) {
              packContractTxs(url, scKey, enKey, maxSize, contractTxs, rulesJson, Some(blockResultJson), state, true)
            }

            // check for normal transactions
            normalTxs.map {
              case (txType, txTup) if txTup.size > 0 =>
                packNormalTxs(url, scKey, enKey, maxSize, e.eventRules, txTup, txType, rulesJson, Some(blockResultJson), state, true)
              case _ => Unit
            }

          case e: BlockRollbackEventSettings if (evokeFrom == "removeAfter")=>
            val height = getHeight(blockDiff)
            val accRule = e.eventRules.filter(r => r.isInstanceOf[RelatedAccs])
            val withTxRule = e.eventRules.filter(r => r.isInstanceOf[WithTxsOfTypes] || r.isInstanceOf[WithTxsOfAccs])
            val withStateRule = e.eventRules.filter(r => r.isInstanceOf[WithStateOfAccs])
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff)
            val ctTxs = extractTxs(re, 9)
            val normalTxs = Seq(2, 5).map(num => extractTxs(re, num))



            val uuid = randomUUID
            val rulesJson = formRuleJson(e.eventRules)
            val txJson = formTxJson(re)
            val toHeight = height + blockDiff.heightDiff
            val payload: JsValue = Json.obj(
              "toHeight" -> toHeight,
              "originHeight" -> height,
              "diffHeight" -> blockDiff.heightDiff,
              "robackTxs" -> txJson
            )

            val subscribeData: JsObject = Json.obj(
              "type" -> 4,
              "uuid" -> uuid,
              "payload" -> payload,
              "referringRules" -> rulesJson
            )
            
            eventWriter ! BlockRollbackEvent(url, scKey, enKey, maxSize, subscribeData)

          case _ =>
            if (evokeFrom == "invalidRollbackHeight") {
              println("************** Too Many Block Rollbacks **************")
            }
        }
      }
    }
  }

  private[events] def filterTxs(rules: Seq[WebhookEventRules], blockTime: Long, blockDiff: BlockDiff): List[(Long, ProcessedTransaction, Seq[String])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aTuple => aTuple match {case (id, (h, tx, accs)) =>
          if (tx.transaction.transactionType == TransactionType.ExecuteContractFunctionTransaction) {
            val combineAccs = accToStr(accs) ++ Seq(tx.transaction.asInstanceOf[ExecuteContractFunctionTransaction].contractId.toString)
            rule.applyRule(h.toLong, blockTime, tx, combineAccs)
          } else {
            rule.applyRule(h.toLong, blockTime, tx, accToStr(accs))
          }
        }
      )).collect {case (id, (h, tx, accs)) => (h.toLong, tx, accToStr(accs))}
  }

  private def extractTxs(txsList: List[(Long, ProcessedTransaction, Seq[String])], txType: Int): List[(Long, ProcessedTransaction, Seq[String])] = {
    txsList.filter {aTuple => aTuple match {case (h, tx, accs) =>
        tx.transaction.transactionType.txType == txType
      }
    }
  }

  private def extractFuncData[T](txFuncTups: Seq[(ProcessedTransaction, Option[FuncDataStruct])]): Seq[(ProcessedTransaction, T)] = {
    txFuncTups.collect {case (tx, Some(fd: T)) => (tx, fd)}
  }

  private def accToStr(accs: Set[Address]): Seq[String] = {
    accs.toSeq.map(_.toString)
  }

  private def getHeight(blockDiff: BlockDiff): Int = {
    blockDiff.txsDiff.transactions.toList.head match {
      case (id, (h, tx, accs)) => h
      case _ => 0
    }
  }

  private def formTxJson(eventData: List[(Long, ProcessedTransaction, Seq[String])]): Seq[JsObject] = {
    eventData.foldLeft(Seq[JsObject]())((accum, aTup) =>
      aTup match {
        case (h, tx, accs) => accum ++ Seq(tx.json)
        case _ => accum
      }
    )
  }

  private def formRuleJson(rules: Seq[WebhookEventRules]): JsObject = {
    rules.foldLeft(Json.obj())((accum, aRule) =>
      accum ++ aRule.json
    )
  }

  private def sumNormalDiff(pTxs: Seq[ProcessedTransaction]): (Long, Seq[JsObject]) = {
    val amt = pTxs.map(_.transaction.asInstanceOf[AmountInvolved].amount).sum
    val txJson = pTxs.foldLeft(Seq[JsObject]())((accum, pTx) => accum ++ Seq(pTx.json))
    (amt, txJson)
  }

  private def sumContractDiff(tupList: Seq[(ProcessedTransaction, FuncDataStruct)]): (Long, Seq[JsObject]) = {
    val amt = tupList.map(tup => tup match {case (tx, func: FuncAmtInvolved) => func.amount}).sum
    val txJson = tupList.foldLeft(Seq[JsObject]())((accum, tup) => tup match {case (tx, _) => accum ++ Seq(tx.json)})
    (amt, txJson)
  }

  private def validToDispatch(pTx: ProcessedTransaction, accRule: Seq[WebhookEventRules], addr: String): Boolean = {
    accRule.size == 0 || accRule.size > 0 && accRule(0).applyRule(0, 0, pTx, Seq(addr))
  }

  private def packContractTxs(
    url: String,
    scKey: Option[String],
    enKey: Option[String],
    maxSize: Int,
    contractTxs: List[(Long, ProcessedTransaction, Seq[String])], 
    rulesJson: JsObject,
    blockResultJson: Option[JsObject],
    state: StateReader,
    addOn: Boolean
  ) = {

    val txFuncTups = contractTxs.map(tup => TransactionHelper.execTxsFuncDataParser(tup._2, state))
    val sendFuncList = extractFuncData[SendFuncData](txFuncTups)
    val destroyFuncList = extractFuncData[DestroyFuncData](txFuncTups)
    val issueFuncList = extractFuncData[IssueFuncData](txFuncTups)
    val funcList =  Seq(sendFuncList, issueFuncList, destroyFuncList).map(funcs => funcs.groupBy {case (_, fd) => (fd.signer, fd.tokenId)})
    val receiverFuncs = sendFuncList.groupBy{case (_, fd) => (fd.recipient, fd.tokenId)}
    
    val blockJson = blockResultJson match {
      case Some(data) => Json.obj("sourceBlocks" -> data)
      case None => JsObject.empty
    }
    val comPayload: JsObject = Json.obj(
      "contract" -> true
    ) ++ blockJson

    for (funcList <- funcList) {
      for ((key, tupList) <- funcList) {
        val newBalance = key match {case (acc, tokenId) => TransactionHelper.getTokenBalance(acc, tokenId, state)}
        val (amtOut, txOutJson) = sumContractDiff(tupList)
        val uuid = randomUUID
        val fd = tupList(0)._2
        val payload: JsObject = Json.obj(
          "address" -> fd.signer,
          "contractId" -> fd.contractId,
          "tokenId" -> fd.tokenId,
          "newBalance" -> newBalance,
        )

        if (receiverFuncs.contains(key)) {
          val (amtIn, txInJson) = sumContractDiff(receiverFuncs(key))
          val amtDiff =  if (addOn) {amtIn - amtOut} else {amtOut - amtIn}

          val extraLoad: JsObject = Json.obj(
            "balanceDiff" -> amtDiff,
            "sourceTxs" -> JsArray(txInJson ++ txOutJson)
          )
          val subscribeData: JsObject = Json.obj(
            "type" -> 3,
            "uuid" -> uuid,
            "referringRules" -> rulesJson,
            "payload" -> (payload ++ extraLoad)
          )

          eventWriter ! StateUpdatedEvent(url, scKey, enKey, maxSize, subscribeData)
        } else {
          val amtDiff = if (addOn) {- amtOut} else {amtOut}
          val load: JsObject = Json.obj(
            "balanceDiff" -> amtDiff,
            "sourceTxs" -> txOutJson
          )
          val subscribeData: JsObject = Json.obj(
            "type" -> 3,
            "uuid" -> uuid,
            "referringRules" -> rulesJson,
            "payload" -> (payload ++ load)
          )

          eventWriter ! StateUpdatedEvent(url, scKey, enKey, maxSize, subscribeData)
        }
      }
    }

    for ((key, tupList) <- receiverFuncs) {
      val newBalance = key match {case (acc, tokenId) => TransactionHelper.getTokenBalance(acc, tokenId, state)}
      val (amtIn, txJson) = sumContractDiff(tupList)
      val amtDiff = if (addOn) {amtIn} else {- amtIn}
      val uuid = randomUUID
      val fd = tupList(0)._2
      val payload: JsObject = Json.obj(
        "contract" -> true,
        "address" -> fd.recipient,
        "contractId" -> fd.contractId,
        "tokenId" -> fd.tokenId,
        "newBalance" -> newBalance,
        "sourceBlocks" -> blockResultJson,
        "balanceDiff" -> amtDiff,
        "sourceTxs" -> txJson
      )

      val subscribeData: JsObject = Json.obj(
        "type" -> 3,
        "uuid" -> uuid,
        "referringRules" -> rulesJson,
        "payload" -> payload
      )

      eventWriter ! StateUpdatedEvent(url, scKey, enKey, maxSize, subscribeData)
    }
  }

  private def packNormalTxs(
    url: String,
    scKey: Option[String],
    enKey: Option[String],
    maxSize: Int,
    eventRules: Seq[WebhookEventRules],
    normalTxs: List[(Long, ProcessedTransaction, Seq[String])],
    txType: Int,
    rulesJson: JsObject,
    blockResultJson: Option[JsObject],
    state: StateReader,
    addOn: Boolean 
  ) = {
    
    if (txType == 2) {
      val txs = normalTxs.map {case (_, pTx, _) => pTx}
      val receiverTxs = txs.groupBy {pTx => pTx.transaction.asInstanceOf[PaymentTransaction].recipient.address}
      val senderTxs = txs.groupBy {pTx => (pTx.transaction.asInstanceOf[PaymentTransaction].proofs.proofs(0).json \ "address").as[String]}
      val accRule = eventRules.filter(r => r.isInstanceOf[RelatedAccs])
      
      val blockJson: JsObject = blockResultJson match {
        case Some(data) => Json.obj("sourceBlocks" -> data)
        case None => JsObject.empty
      }
      val comPayload: JsObject = Json.obj(
        "contract" -> false,
        "sourceBlocks" -> blockResultJson
      )
      for ((addr, pTxs) <- senderTxs) {
        if (validToDispatch(pTxs(0), accRule, addr)) {
          val newBalance = TransactionHelper.getBalance(addr, state)
          val (amtOut, txJson) = sumNormalDiff(pTxs)
          val uuid = randomUUID

          if (receiverTxs.contains(addr)) {
            val txsIn = receiverTxs(addr)
            val (amtIn, txInJson) = sumNormalDiff(txsIn)
            val amtDiff = if (addOn) {amtIn - amtOut} else {amtOut - amtIn}
            val resultTxJson = txJson ++ txInJson
            val payload: JsObject = Json.obj(
              "address" -> addr,
              "balanceDiff" -> amtDiff,
              "newBalance" -> newBalance,
              "sourceTxs" -> resultTxJson
            ) ++ comPayload

            val subscribeData: JsObject = Json.obj(
              "type" -> 3,
              "uuid" -> uuid,
              "payload" -> payload,
              "referringRules" -> rulesJson
            )

            println(subscribeData)

          } else {
            val amtDiff = if (addOn) {- amtOut} else {amtOut}
            val payload: JsObject = Json.obj(
              "addr" -> addr,
              "balanceDiff" -> amtDiff,
              "newBalance" -> newBalance,
              "sourceTxs" -> txJson
            ) ++ comPayload

            val subscribeData: JsObject = Json.obj(
              "type" -> 3,
              "uuid" -> uuid,
              "payload" -> payload,
              "referringRules" -> rulesJson
            )

            println(subscribeData)
          }
        }
      }

      for ((addr, pTxs) <- receiverTxs) {
        if (validToDispatch(pTxs(0), accRule, addr)) {
          val newBalance = TransactionHelper.getBalance(addr, state)
          val (amtIn, txJson) = sumNormalDiff(pTxs)
          val amtDiff = if (addOn) {amtIn} else {- amtIn}
          val uuid = randomUUID

          val payload: JsObject = Json.obj(
            "addr" -> addr,
            "balanceDiff" -> amtDiff,
            "newBalance" -> newBalance,
            "sourceTxs" -> txJson
          ) ++ comPayload

          val subscribeData: JsObject = Json.obj(
            "type" -> 3,
            "uuid" -> uuid,
            "payload" -> payload,
            "referringRules" -> rulesJson
          )
          println("##### receiver side !!!!")
          println(subscribeData)
        }
      }

    } else if (txType == 5) {
      val grouped = normalTxs.groupBy {case (_, pTx, _) => pTx.transaction.asInstanceOf[MintingTransaction].recipient.address}
      for ((addr, tupList) <- grouped) {
        val txs = tupList.map {case (_, pTx, _) => pTx}
        val (amt, txJson) = sumNormalDiff(txs)
        val amtDiff = if (addOn) {amt} else {- amt}
        val uuid = randomUUID
        val payload: JsObject = Json.obj(
          "contract" -> false,
          "address" -> addr,
          "balanceDiff" -> amtDiff,
          "newBalance" -> TransactionHelper.getBalance(addr, state),
          "sourceTxs" -> txJson,
          "sourceBlocks" -> blockResultJson
        )

        val subscribeData: JsObject = Json.obj(
          "type" -> 3,
          "uuid" -> uuid,
          "referringRules" -> rulesJson,
          "payload" -> payload
        )

        eventWriter ! StateUpdatedEvent(url, scKey, enKey, maxSize, subscribeData)
      }
    }
  }

}

object EventTriggers {
  def apply(eventWriter: ActorRef, eventSetting: EventSettings): EventTriggers = {
    new EventTriggers(eventWriter, eventSetting)
  }
}