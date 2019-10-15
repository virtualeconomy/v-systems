package vsys.actor

import akka.actor.{ActorSystem, AllForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}
import com.typesafe.config.Config
import vsys.utils.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.DynamicVariable

object RootActorSystem extends ScorexLogging {
  private val failed = new DynamicVariable(false)

  final class EscalatingStrategy extends SupervisorStrategyConfigurator {
    override def create(): SupervisorStrategy = AllForOneStrategy(loggingEnabled = false) {
      case t: Throwable =>
        failed.value = true
        log.error("Root actor got exception, escalate", t)
        SupervisorStrategy.Escalate
    }
  }

  def start(id: String, config: Config)(init: ActorSystem => Unit): Unit = {
    val system = ActorSystem(id, config)
    try {
      init(system)
    } catch {
      case e: Exception =>
        log.error(s"Error while initializing actor system $id", e)
        sys.exit(1)
    }

    Await.result(system.whenTerminated, Duration.Inf)
    if (failed.value) {
      sys.exit(1)
    } else {
      sys.exit(0)
    }
  }
}
