package vsys.utils

import org.h2.mvstore.MVMap
import org.h2.mvstore.`type`.ObjectDataType

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}

class LogMVMapBuilder[K, V] extends MVMap.Builder[K, V] with ScorexLogging {
  override def create(): MVMap[K, V] = {
    if (keyType == null) keyType = new ObjectDataType
    if (valueType == null) valueType = new ObjectDataType
    new MVMap[K, V](keyType, valueType) {
      override def put(key: K, value: V): V = {
        Try { super.put(key, value) } match {
          case Success(v) => v
          case Failure(t: Throwable) => {
            log.error("MVStore put error", t)
            forceStopApplication()
            throw t
          }
        }
      }
      override def get(key: scala.Any): V = {
        Try { super.get(key) } match {
          case Success(v) => v
          case Failure(t: Throwable) => {
            log.error("MVStore get error", t)
            forceStopApplication()
            throw t
          }
        }
      }
    }
  }
}
