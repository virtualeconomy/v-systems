package vsys.events

import vsys.utils.ScorexLogging
import vsys.settings.{EventSettings, BlockAppendedEventSettings, TxConfirmedEventSettings}
import vsys.settings.{BlockRollbackEventSettings, StateUpdatedEventSettings, WebhookEventRules}
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.blockchain.state.{BlockDiff, ByteStr}
import vsys.blockchain.block.Block
import vsys.blockchain.contract.DataEntry
import vsys.account.{Address, ContractAccount}
import akka.actor.ActorRef
import play.api.libs.json.{JsObject, JsValue, Json}
import java.util.UUID.randomUUID

class EventTriggers(eventWriter: ActorRef, eventSetting: EventSettings) extends ScorexLogging {

  // TO DO: Should handle more webhook event settings
  def evokeWebhook(block: Block, blockDiff: BlockDiff, evokeFrom: String): Unit = {
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
            val ctRe = filterContracts(e.eventRules, block.timestamp, blockDiff)
            val txJson = formTxJson(re)



          case e: BlockRollbackEventSettings if (evokeFrom == "removeAfter")=>
            val height = getHeight(blockDiff)
            val re = filterTxs(e.eventRules, block.timestamp, blockDiff)
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
            
            eventWriter ! subscribeData

          case _ =>
            if (evokeFrom == "invalidRollbackHeight") {
              println("lululululullulullu **************")
            }
        }
      }
    }
  }

  private[events] def filterTxs(rules: Seq[WebhookEventRules], blockTime: Long, blockDiff: BlockDiff): List[(Long, ProcessedTransaction, Seq[String])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aTuple => aTuple match {case (id, (h, tx, accs)) => rule.applyRule(h.toLong, blockTime, tx, accToStr(accs))}
      )).collect {case (id, (h, tx, accs)) => (h.toLong, tx, accToStr(accs))}
  }

  private def filterContracts(rules: Seq[WebhookEventRules], blockTime: Long, blockDiff: BlockDiff): List[(Long, ProcessedTransaction, ByteStr, ByteStr, Seq[String])]= {
    val txMap = blockDiff.txsDiff.transactions
    val contracts = blockDiff.txsDiff.contracts
    val contractTokens = blockDiff.txsDiff.contractTokens
    rules.foldLeft(contracts.toList)((accum, rule) =>
      accum.filter(aTuple => aTuple match {case (ctId: ByteStr, (h: Int, txId: ByteStr, _, _)) => rule.applyRule(h.toLong, blockTime, (getTx _).tupled(txMap(txId)), Seq(ctId.toString))}
      )).collect {
      case (ctId: ByteStr, (h: Int, txId: ByteStr, _, accs: Set[Address])) =>
        val pTx = (getTx _).tupled(txMap(txId))
        pTx.transaction match {
          case p: ExecuteContractFunctionTransaction =>
            val tokenId = ContractAccount.tokenIdFromBytes(ctId.arr, Array(contractTokens(ctId).toByte)).right.get
            (h.toLong, pTx, ctId, tokenId, accToStr(accs))
          case _ => (0L, pTx, ctId, ctId, Seq.empty)
        }
      }.filter {finTup => finTup match {case (h: Long, _, _, _, _) => h != 0}}
  }

  private def accToStr(accs: Set[Address]): Seq[String] = {
    accs.toSeq.map(_.toString)
  }

  private def getHeight(blockDiff: BlockDiff): Int = {
    val height = blockDiff.txsDiff.transactions.toList.head match {
      case (id, (h, tx, accs)) => h
      case _ => 0
    }
    height
  }

  private def getTx(h: Int, tx: ProcessedTransaction, accs: Set[Address]): ProcessedTransaction = tx

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
}

object EventTriggers {
  def apply(eventWriter: ActorRef, eventSetting: EventSettings): EventTriggers = {
    new EventTriggers(eventWriter, eventSetting)
  }
}