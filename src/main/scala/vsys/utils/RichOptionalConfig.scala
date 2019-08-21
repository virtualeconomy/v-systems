package vsys.utils

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._


object RichOptionalConfig {
	implicit class RichConfigVal(val config: Config) extends AnyVal {

		def getConfBoolean(path: String, defaultVal: Boolean): Boolean = if (config.hasPath(path)) {
			config.as[Boolean](path)
		}else{
			defaultVal
		}

		def getConfInt(path: String, defaultVal: Int): Int = if (config.hasPath(path)){
			config.as[Int](path)
		}else{
			defaultVal
		}

		def getConfLong(path: String, defaultVal: Long): Long = if (config.hasPath(path)){
			config.as[Long](path)
		}else{
			defaultVal
		}

		def getConfString(path: String, defaultVal: String): String = if (config.hasPath(path)){
			config.as[String](path)
		}else{
			defaultVal
		}
	}

	implicit class RichConfigRef(val config: Config) extends AnyRef {

		def getConfSeqInt(path: String, defaultVal: Seq[Int]): Seq[Int] = if (config.hasPath(path)){
			config.as[Seq[Int]](path)
		}else{
			defaultVal
		}

		def getConfSeqString(path: String, defaultVal: Seq[String]): Seq[String] = if (config.hasPath(path)){
			config.as[Seq[String]](path)
		}else{
			defaultVal
		}
	}
}