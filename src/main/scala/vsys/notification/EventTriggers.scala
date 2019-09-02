package vsys.notification

import vsys.blockchain.block.Block
import vsys.settings.WebhookSettings

trait EventTriggers {
  def validateBlockTrigger(block: Block, webhookSetting: WebhookSettings): Unit
  def validateStateTrigger(blockDiff: BlockDiff, webhookSetting: WebhookSettings): Unit
}

object EventTriggers {
  def validateBlockTrigger(block: Block, webhookSetting: WebhookSettings): Unit = {

    val settings = webhookSetting.events.filter(_.typeId != 3)
  }

  def validateStateTrigger(blockDiff: BlockDiff, webhookSetting: WebhookSettings): Unit = {

  }
}