package vsys.events

import vsys.utils.{ScorexLogging, TransactionHelper}
import vsys.settings.{EventSettings, BlockAppendedEventSettings, TxConfirmedEventSettings, WebhookSettings}
import vsys.settings.{BlockRollbackEventSettings, StateUpdatedEventSettings, WebhookEventRules}
import vsys.blockchain.transaction.{ProcessedTransaction, PaymentTransaction, MintingTransaction, GenesisTransaction}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

import vsys.blockchain.state.BlockDiff
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.block.Block
import vsys.blockchain.contract.{FuncDataStruct, SendFuncData}
import vsys.blockchain.contract.{IssueFuncData, DestroyFuncData}
import vsys.account.Address
import vsys.settings.{RelatedAccs, WithTxsOfTypes, WithTxsOfAccs, WithStateOfAccs}

import akka.actor.ActorRef
import play.api.libs.json.{JsObject, JsValue, Json}
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
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff, state)
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

              eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
            }

          case e: TxConfirmedEventSettings if (evokeFrom == "processBlock") =>
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff, state)
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
              eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
            }

          case e: StateUpdatedEventSettings if (evokeFrom == "processBlock") =>
            val height = getHeight(blockDiff)
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff, state)

            if (re.size > 0) {
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
                packContractTxs(webhookSetting, maxSize, contractTxs, rulesJson, Some(blockResultJson), state)
              }

              // check for normal transactions
              normalTxs.map {
                case (txType, txTup) if txTup.size > 0 =>
                  packNormalTxs(webhookSetting, maxSize, e.eventRules, txTup, txType, rulesJson, Some(blockResultJson), state)
                case _ => Unit
              }
            }

          case e: BlockRollbackEventSettings if (evokeFrom == "removeAfter")=>
            val height = getHeight(blockDiff)
            val uuid = randomUUID
            val accRule = e.eventRules.filter(r => r.isInstanceOf[RelatedAccs])
            val withTxRule = e.eventRules.filter(r => r.isInstanceOf[WithTxsOfTypes] || r.isInstanceOf[WithTxsOfAccs])
            val withStateRule = e.eventRules.filter(r => r.isInstanceOf[WithStateOfAccs])
            val re = filterTxs(accRule, block.timestamp, blockDiff, state)

            if (re.size > 0) {
              val ctTxs = extractTxs(re, 9)
              val normalTxs = Seq(2, 5).map(num => (num, extractTxs(re, num)))
              val robackTxs = filterTxs(withTxRule ++ accRule, block.timestamp, blockDiff, state)
              val robackTxsJson = formTxJson(robackTxs)

              val (funcList, recvFuncs) = groupCtTxs(ctTxs, state)
              val ctStateJson = formCtStateJson(funcList, recvFuncs, withStateRule, state)
              val extraStateJson = recvFuncs match {
                case Some(recvMap) => formCtStateJson(Seq(recvMap), None, withStateRule, state)
                case None => Seq.empty[JsObject]
              }

              val normalTxStateJson = normalTxs.foldLeft(Seq[JsObject]()) {(accum, tup) =>
                val (num, txTups) = tup
                accum ++ formNormalStateJson(num, txTups, withStateRule, state)
              }

              val rulesJson = formRuleJson(e.eventRules)
              val toHeight = height + blockDiff.heightDiff
              val robackPayload = if (robackTxs.nonEmpty) {
                Json.obj("robackTxs" -> robackTxsJson)
              } else {
                JsObject.empty
              }

              val stateJson = normalTxStateJson ++ ctStateJson ++ extraStateJson
              val statePayload = if (stateJson.nonEmpty) {
                Json.obj("robackBalance" -> stateJson)
              } else {
                JsObject.empty
              }

              val payload: JsObject = Json.obj(
                "toHeight" -> toHeight,
                "originHeight" -> height,
                "diffHeight" -> blockDiff.heightDiff
              ) ++ statePayload ++ robackPayload

              val subscribeData: JsObject = Json.obj(
                "type" -> 4,
                "uuid" -> uuid,
                "payload" -> payload,
                "referringRules" -> rulesJson
              )

              eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
            }

          case _ => Unit
        }
      }
    }
  }

  private[events] def filterTxs(rules: Seq[WebhookEventRules], blockTime: Long, blockDiff: BlockDiff, state: StateReader): List[(Long, ProcessedTransaction, Seq[String])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aTuple => aTuple match {case (id, (h, pTx, accs)) =>
          if (accs.isEmpty) {
            val combineAccs = TransactionHelper.getTransactionAccs(pTx, state)
            rule.applyRule(h.toLong, blockTime, pTx, combineAccs)
          } else {
            val ctId = pTx.transaction match {
              case e: ExecuteContractFunctionTransaction => Seq(e.contractId.address)
              case e: RegisterContractTransaction => Seq(e.contractId.address)
              case _ => Seq.empty
            }

            rule.applyRule(h.toLong, blockTime, pTx, accToStr(accs) ++ ctId)
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

  private def sumNormalDiff(pTxs: Seq[ProcessedTransaction], isSelf: Boolean): (Long, Seq[JsObject]) = {
    val amt = pTxs.map {pTx =>
      pTx.transaction match {
        case tx: GenesisTransaction => tx.amount
        case tx: PaymentTransaction => if (isSelf) {- tx.amount - pTx.feeCharged} else { tx.amount }
        case tx: MintingTransaction => tx.amount
        case _ => if (isSelf) { pTx.transaction.transactionFee } else { 0 }
      }
    }.sum
    val txJson = pTxs.foldLeft(Seq[JsObject]())((accum, pTx) => accum ++ Seq(pTx.json))
    (amt, txJson)
  }

  private def sumContractDiff(tupList: Seq[(ProcessedTransaction, FuncDataStruct)], isSelf: Boolean): (Long, Seq[JsObject]) = {
    val factor = if (isSelf) { 1 } else { -1 }
    val amt = tupList.map {tup =>
      val (_, fd) = tup
      fd match {
        case d: IssueFuncData => d.amount
        case d: DestroyFuncData => - d.amount
        case d: SendFuncData => - d.amount * factor
        case _ => 0
      }
    }.sum

    val txJson = tupList.foldLeft(Seq[JsObject]())((accum, tup) => tup match {case (tx, _) => accum ++ Seq(tx.json)})
    (amt, txJson)
  }

  private def isValidToProceed(
    pTx: ProcessedTransaction,
    accRule: Seq[WebhookEventRules],
    addrs: Seq[String]
  ): Boolean = {
    accRule.size == 0 || accRule.size > 0 && accRule(0).applyRule(0, 0, pTx, addrs)
  }

  private def groupCtTxs(contractTxs: List[(Long, ProcessedTransaction, Seq[String])], state: StateReader): (
    Seq[Map[(String, String, String), Seq[(ProcessedTransaction, FuncDataStruct)]]],
    Option[Map[(String, String, String), Seq[(ProcessedTransaction, FuncDataStruct)]]]
  ) = {
    val txFuncTupMap = contractTxs.map(tup => TransactionHelper.execTxsFuncDataParser(tup._2, state))
      .flatten
      .groupBy(_._2.funcName)

    val funcList =  Seq("send", "issue", "destroy").flatMap(txFuncTupMap.get(_)).map {tups =>
      tups.groupBy{tup =>
        val (_, fd) = tup
        (fd.signer, fd.tokenId, fd.funcName)
      }
    }

    val receiverFuncs = txFuncTupMap.get("send").map {tups =>
      tups.groupBy{tup =>
        val fd = tup._2.asInstanceOf[SendFuncData]
        (fd.recipient, fd.tokenId, fd.funcName)
      }
    }
    (funcList, receiverFuncs)
  }

  private def packContractTxs(
    webhookSetting: WebhookSettings,
    maxSize: Int,
    contractTxs: List[(Long, ProcessedTransaction, Seq[String])],
    rulesJson: JsObject,
    blockResultJson: Option[JsObject],
    state: StateReader
  ) = {
    val url = webhookSetting.url
    val scKey = webhookSetting.secretKey
    val enKey = webhookSetting.encryptKey

    val (funcList, receiverFuncs) = groupCtTxs(contractTxs, state)

    val blockJson = blockResultJson match {
      case Some(data) => Json.obj("sourceBlocks" -> data)
      case None => JsObject.empty
    }
    val comPayload: JsObject = Json.obj(
      "contract" -> true
    ) ++ blockJson

    for (funcs <- funcList) {
      for ((key, tupList) <- funcs) {
        val (acc, tokenId, _) = key
        val oldBalance = TransactionHelper.getTokenBalance(acc, tokenId, state)
        val (amtOut, txOutJson) = sumContractDiff(tupList, true)
        val uuid = randomUUID
        val fd = tupList(0)._2
        val payload: JsObject = Json.obj(
          "address" -> acc,
          "contractId" -> fd.contractId,
          "tokenId" -> tokenId
        ) ++ comPayload

        receiverFuncs match {
          case Some(recvMap) if recvMap.contains(key) =>
            val (amtIn, txInJson) = sumContractDiff(recvMap(key), true)
            val amtDiff = amtIn - amtOut

            val extraLoad: JsObject = Json.obj(
              "balanceDiff" -> amtDiff,
              "newBalance" -> (oldBalance + amtDiff),
              "sourceTxs" -> (txInJson ++ txOutJson)
            )
            val subscribeData: JsObject = Json.obj(
              "type" -> 3,
              "uuid" -> uuid,
              "referringRules" -> rulesJson,
              "payload" -> (payload ++ extraLoad)
            )

            eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)

          case _ =>
            val load: JsObject = Json.obj(
              "balanceDiff" -> - amtOut,
              "newBalance" -> (oldBalance - amtOut),
              "sourceTxs" -> txOutJson
            )
            val subscribeData: JsObject = Json.obj(
              "type" -> 3,
              "uuid" -> uuid,
              "referringRules" -> rulesJson,
              "payload" -> (payload ++ load)
            )

            eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
        }
      }
    }

    for ((key, tupList) <- receiverFuncs.getOrElse(Map.empty)) {
      val (acc, tokenId, _) = key
      val oldBalance = TransactionHelper.getTokenBalance(acc, tokenId, state)
      val (amtIn, txJson) = sumContractDiff(tupList, false)
      val uuid = randomUUID
      val fd = tupList(0)._2.asInstanceOf[SendFuncData]
      val payload: JsObject = Json.obj(
        "address" -> acc,
        "contractId" -> fd.contractId,
        "tokenId" -> tokenId,
        "newBalance" -> (oldBalance + amtIn),
        "balanceDiff" -> amtIn,
        "sourceTxs" -> txJson
      ) ++ comPayload

      val subscribeData: JsObject = Json.obj(
        "type" -> 3,
        "uuid" -> uuid,
        "referringRules" -> rulesJson,
        "payload" -> payload
      )

      eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
    }
  }

  private def packNormalTxs(
    webhookSetting: WebhookSettings,
    maxSize: Int,
    eventRules: Seq[WebhookEventRules],
    normalTxs: List[(Long, ProcessedTransaction, Seq[String])],
    txType: Int,
    rulesJson: JsObject,
    blockResultJson: Option[JsObject],
    state: StateReader
  ) = {
    val url = webhookSetting.url
    val scKey = webhookSetting.secretKey
    val enKey = webhookSetting.encryptKey

    val blockJson: JsObject = blockResultJson match {
      case Some(data) => Json.obj("sourceBlocks" -> data)
      case None => JsObject.empty
    }
    val comPayload: JsObject = Json.obj(
      "contract" -> false
    ) ++ blockJson
    val txs = normalTxs.map {case (_, pTx, _) => pTx}

    txType match {
      case 2 =>
        val receiverTxs = txs.groupBy {pTx => pTx.transaction.asInstanceOf[PaymentTransaction].recipient.address}
        val senderTxs = txs.groupBy {pTx => (pTx.transaction.asInstanceOf[PaymentTransaction].proofs.proofs(0).json \ "address").as[String]}
        val accRule = eventRules.filter(r => r.isInstanceOf[RelatedAccs])

        for ((addr, pTxs) <- senderTxs) {
          if (isValidToProceed(pTxs(0), accRule, Seq(addr))) {
            val oldBalance = TransactionHelper.getBalance(addr, state)
            val (amtOut, txJson) = sumNormalDiff(pTxs, true)
            val uuid = randomUUID

            if (receiverTxs.contains(addr)) {
              val txsIn = receiverTxs(addr)
              val (amtIn, txInJson) = sumNormalDiff(txsIn, false)
              val amtDiff = amtIn - amtOut
              val resultTxJson = txJson ++ txInJson
              val payload: JsObject = Json.obj(
                "address" -> addr,
                "balanceDiff" -> amtDiff,
                "newBalance" -> (oldBalance + amtDiff),
                "sourceTxs" -> resultTxJson
              ) ++ comPayload

              val subscribeData: JsObject = Json.obj(
                "type" -> 3,
                "uuid" -> uuid,
                "payload" -> payload,
                "referringRules" -> rulesJson
              )

              eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)

            } else {
              val payload: JsObject = Json.obj(
                "addr" -> addr,
                "balanceDiff" -> -amtOut,
                "newBalance" -> (oldBalance - amtOut),
                "sourceTxs" -> txJson
              ) ++ comPayload

              val subscribeData: JsObject = Json.obj(
                "type" -> 3,
                "uuid" -> uuid,
                "payload" -> payload,
                "referringRules" -> rulesJson
              )

              eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
            }
          }
        }

        for ((addr, pTxs) <- receiverTxs) {
          if (isValidToProceed(pTxs(0), accRule, Seq(addr))) {
            val oldBalance = TransactionHelper.getBalance(addr, state)
            val (amtIn, txJson) = sumNormalDiff(pTxs, false)
            val uuid = randomUUID

            val payload: JsObject = Json.obj(
              "addr" -> addr,
              "balanceDiff" ->  amtIn,
              "newBalance" -> (oldBalance + amtIn),
              "sourceTxs" -> txJson
            ) ++ comPayload

            val subscribeData: JsObject = Json.obj(
              "type" -> 3,
              "uuid" -> uuid,
              "payload" -> payload,
              "referringRules" -> rulesJson
            )

            eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
          }
        }

      case 5 =>
        val grouped = txs.groupBy {pTx => pTx.transaction.asInstanceOf[MintingTransaction].recipient.address}
        for ((addr, txs) <- grouped) {
          val (amt, txJson) = sumNormalDiff(txs, true)
          val oldBalance = TransactionHelper.getBalance(addr, state)
          val uuid = randomUUID
          val payload: JsObject = Json.obj(
            "address" -> addr,
            "balanceDiff" -> amt,
            "newBalance" -> (oldBalance + amt),
            "sourceTxs" -> txJson
          ) ++ comPayload

          val subscribeData: JsObject = Json.obj(
            "type" -> 3,
            "uuid" -> uuid,
            "referringRules" -> rulesJson,
            "payload" -> payload
          )

          eventWriter ! Event(url, scKey, enKey, maxSize, subscribeData)
        }
    }
  }

  private def formCtStateJson(
    funcList: Seq[Map[(String, String, String), Seq[(ProcessedTransaction, FuncDataStruct)]]],
    recvFuncs: Option[Map[(String, String, String), Seq[(ProcessedTransaction, FuncDataStruct)]]],
    withStateRule: Seq[WebhookEventRules],
    state: StateReader
  ): Seq[JsObject] = {
    val funcListJson = funcList.foldLeft(Seq[JsObject]())((allFuncs, funcs) =>
      funcs.foldLeft(Seq.empty[JsObject]) {(sameFunc, tup) =>
        val (key, tupList) = tup
        val (acc, tokenId, _) = key
        val (pTx, fd) = tupList(0)
        val ctId = fd.contractId
        if (isValidToProceed(pTx, withStateRule, Seq(acc, ctId))) {
          val oldBalance = TransactionHelper.getTokenBalance(acc, tokenId, state)
          val (amtOut, txOutJson) = sumContractDiff(tupList, true)

          recvFuncs match {
            case Some(recvMap) if recvMap.contains(key) =>
              val (amtIn, txInJson) = sumContractDiff(recvMap(key), false)
              val amtDiff = amtOut - amtIn
              val newBalance = oldBalance + amtDiff
              val balancePayload: JsObject = Json.obj(
                "contract" -> true,
                "address" -> acc,
                "contractId" -> ctId,
                "tokenId" -> tokenId,
                "balanceDiff" -> amtDiff,
                "newBalance" -> newBalance
              )

              allFuncs ++ sameFunc ++ Seq(balancePayload)
            case _ =>
              val balancePayload: JsObject = Json.obj(
                "contract" -> true,
                "address" -> acc,
                "contractId" -> ctId,
                "tokenId" -> tokenId,
                "balanceDiff" -> amtOut,
                "newBalance" -> (oldBalance + amtOut)
              )
              allFuncs ++ sameFunc ++ Seq(balancePayload)
          }
        } else {
          allFuncs ++ sameFunc
        }
      }
    )

    recvFuncs match {
      case Some(recvMap) =>
        recvMap.foldLeft(funcListJson) {(accum, pair) =>
          val (key, tups) = pair
          val (pTx, fd) = tups(0)
          val (acc, tokenId, _) = key
          val ctId = fd.contractId

          if (isValidToProceed(pTx, withStateRule, Seq(ctId, acc))) {
            val oldBalance = TransactionHelper.getTokenBalance(acc, tokenId, state)
            val (amtIn, txInJson) = sumContractDiff(tups, false)
            val balancePayload: JsObject = Json.obj(
              "contract" -> true,
              "address" -> acc,
              "contractId" -> ctId,
              "tokenId" -> tokenId,
              "balanceDiff" ->  - amtIn,
              "newBalance" -> (oldBalance - amtIn)
            )

            accum ++ Seq(balancePayload)
          } else {
            accum
          }
        }

      case None => funcListJson
    }
  }

  private def formNormalStateJson(
    txType: Int,
    normalTxs: List[(Long, ProcessedTransaction, Seq[String])],
    withStateRule: Seq[WebhookEventRules],
    state: StateReader
  ): Seq[JsObject] = {
    val txs = normalTxs.map {case (_, pTx, _) => pTx}

    txType match {
      case 2 =>
        val receiverTxs = txs.groupBy {pTx => pTx.transaction.asInstanceOf[PaymentTransaction].recipient.address}
        val senderTxs = txs.groupBy {pTx => (pTx.transaction.asInstanceOf[PaymentTransaction].proofs.proofs(0).json \ "address").as[String]}

        val sendJson = senderTxs.foldLeft(Seq.empty[JsObject]) {(accum, sender) =>
          val (addr, pTxs) = sender
          val comPayload: JsObject = Json.obj(
            "contract" -> false,
            "address" -> addr
          )
          if (isValidToProceed(pTxs(0), withStateRule, Seq(addr))) {
            val oldBalance = TransactionHelper.getBalance(addr, state)
            val (amtOut, txOutJson) = sumNormalDiff(pTxs, true)

            if (receiverTxs.contains(addr)) {
              val recvTxs = receiverTxs(addr)
              val (amtIn, _) = sumNormalDiff(recvTxs, false)
              val amtDiff = amtOut - amtIn
              val balancePayload: JsObject = Json.obj(
                "balanceDiff" -> amtDiff,
                "newBalance" -> (oldBalance + amtDiff)
              ) ++ comPayload
              accum ++ Seq(balancePayload)
            } else {
              val balancePayload: JsObject = Json.obj(
                "balanceDiff" -> amtOut,
                "newBalance" -> (oldBalance + amtOut)
              ) ++ comPayload
              accum ++ Seq(balancePayload)
            }
          } else {
            accum
          }
        }

        receiverTxs.foldLeft(sendJson) {(accum, recv) =>
          val (addr, pTxs) = recv
          if (isValidToProceed(pTxs(0), withStateRule, Seq(addr))) {
            val oldBalance = TransactionHelper.getBalance(addr, state)
            val (amtIn, _) = sumNormalDiff(pTxs, false)
            val balancePayload: JsObject = Json.obj(
              "balanceDiff" -> - amtIn,
              "newBalance" -> (oldBalance - amtIn),
              "contract" -> false,
              "address" -> addr
            )

            accum ++ Seq(balancePayload)
          } else {
            accum
          }
        }

      case 5 =>
        val grouped = txs.groupBy {pTx => pTx.transaction.asInstanceOf[MintingTransaction].recipient.address}
        grouped.foldLeft(Seq.empty[JsObject]) {(accum, mint) =>
          val (addr, pTxs) = mint
          if (isValidToProceed(pTxs(0), withStateRule, Seq(addr))) {
            val oldBalance = TransactionHelper.getBalance(addr, state)
            
            val (amtIn, _) = sumNormalDiff(pTxs, true)
            val balancePayload: JsObject = Json.obj(
              "balanceDiff" -> -amtIn,
              "newBalance" -> (oldBalance - amtIn),
              "contract" -> false,
              "address" -> addr
            )

            accum ++ Seq(balancePayload)
          } else {
            accum
          }
        }

      case _ => Seq.empty[JsObject]
    }
  }

}

object EventTriggers {
  def apply(eventWriter: ActorRef, eventSetting: EventSettings): EventTriggers = {
    new EventTriggers(eventWriter, eventSetting)
  }
}