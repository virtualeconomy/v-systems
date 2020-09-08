package vsys.utils

import com.google.common.util.concurrent.UncheckedExecutionException
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import org.slf4j.{Logger, LoggerFactory}

case class LoggerFacade(logger: Logger) {
  def trace(implicit message: String): Unit = {
    if (logger.isTraceEnabled)
      logger.trace(message)
  }

  def debug(implicit message: String, arg: Any): Unit = {
    if (logger.isDebugEnabled)
      logger.debug(message, arg)
  }

  def debug(implicit message: String): Unit = {
    if (logger.isDebugEnabled)
      logger.debug(message)
  }

  def info(implicit message: String): Unit = {
    if (logger.isInfoEnabled)
      logger.info(message)
  }

  def info(implicit message: String, arg: Any): Unit = {
    if (logger.isInfoEnabled)
      logger.info(message, arg)
  }

  def info(implicit message: String, throwable: Throwable): Unit = {
    if (logger.isInfoEnabled)
      logger.info(message, throwable)
  }

  def warn(implicit message: String): Unit = {
    if (logger.isWarnEnabled)
      logger.warn(message)
  }

  def warn(implicit message: String, throwable: Throwable): Unit = {
    if (logger.isWarnEnabled)
      logger.warn(message, throwable)
  }

  def error(implicit message: String): Unit = {
    if (logger.isErrorEnabled)
      logger.error(message)
  }

  def error(implicit message: String, throwable: Throwable): Unit = {
    if (logger.isErrorEnabled)
      logger.error(message, throwable)
  }
}

trait ScorexLogging {
  protected def log = LoggerFacade(LoggerFactory.getLogger(this.getClass))

  implicit class TaskExt[A](t: Task[A]) {
    def runAsyncLogErr(implicit s: Scheduler): CancelableFuture[A] = logErr.runAsync

    def logErr: Task[A] = {
      t.onErrorHandleWith(ex => {
        log.error(s"Error executing task", ex)
        Task.raiseError[A](ex)
      })
    }

    def logErrDiscardNoSuchElementException: Task[A] = {
      t.onErrorHandleWith(ex => {
        ex match {
          case gex: UncheckedExecutionException =>
            Option(gex.getCause) match {
              case Some(nseex: NoSuchElementException) =>
              case _                                   => log.error(s"Error executing task", ex)
            }
          case _ => log.error(s"Error executing task", ex)
        }
        Task.raiseError[A](ex)
      })
    }
  }

  implicit class ObservableExt[A](o: Observable[A]) {

    def logErr: Observable[A] = {
      o.onErrorHandleWith(ex => {
        log.error(s"Error observing item", ex)
        Observable.raiseError[A](ex)
      })
    }
  }
}
