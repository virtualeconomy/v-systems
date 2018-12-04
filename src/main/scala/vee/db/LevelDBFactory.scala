package vee.db

import org.iq80.leveldb.DBFactory
import scorex.utils.ScorexLogging

object LevelDBFactory extends ScorexLogging {
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = load

  private def load: DBFactory = {
    val loaders = Seq(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader)

    val names = Seq(javaFactory)

    val pairs = names.flatMap(x => loaders.map(y => (x, y)))

    pairs.view
      .flatMap {
        case (name, loader) =>
          try {
            val c = loader.loadClass(name).getConstructor().newInstance().asInstanceOf[DBFactory]
            log.trace(s"Loaded ${c.getClass.getName} with $loader")
            Some(c)
          } catch {
            case _: Throwable =>
              None
          }
      }
      .headOption
      .getOrElse(throw new Exception(s"Could not load the factory classes: $javaFactory"))
  }
}