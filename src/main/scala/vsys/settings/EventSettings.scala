package vsys.settings

import com.typesafe.config.Config


case class EventSettings(webhookSettings: Seq[WebhookSettings])


object EventSettings {
	import vsys.utils.RichOptionalConfig._

	val configPath = "vsys.Event"

	def fromConfig(config: Config): EventSettings = {
		if(!config.getConfBoolean(s"$configPath.enable", false)){
			//no sending any event notification
			EventSettings(Seq.empty)
		}else{
			val webhookSettings = WebhookSettings.fromConfig(config)
			EventSettings(webhookSettings)
		}
	}
}