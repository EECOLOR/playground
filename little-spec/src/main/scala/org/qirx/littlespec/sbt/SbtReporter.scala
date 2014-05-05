package org.qirx.littlespec.sbt

import sbt.testing.EventHandler
import sbt.testing.Logger
import sbt.testing.TaskDef
import org.qirx.littlespec.Result
import sbt.testing.EventHandler
import sbt.testing.Logger
import sbt.testing.Event
import sbt.testing.TaskDef
import sbt.testing.Fingerprint
import sbt.testing.Selector
import sbt.testing.Status
import sbt.testing.OptionalThrowable
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import org.qirx.littlespec.Pending
import org.qirx.littlespec.CompoundResult
import org.qirx.littlespec.Failure
import org.qirx.littlespec.Success
import org.qirx.littlespec.Result
import org.qirx.littlespec.UnexpectedFailure

trait SbtReporter {
  def report(taskDef: TaskDef, eventHandler: EventHandler, loggers: Seq[Logger], results: Seq[Result]): Unit
}

class DefaultSbtReporter extends SbtReporter {
  def report(taskDef: TaskDef, eventHandler: EventHandler, loggers: Seq[Logger], results: Seq[Result]): Unit = {

    val event = eventFor(taskDef, eventHandler) _
    val logLevel = logStringFor(loggers) _

    def report(results: Seq[Result], level: Int): Unit = {
      val log = logLevel(level)

      results.foreach {
        case CompoundResult(title, results) =>
          log(_.info, title)
          report(results, level + 1)
        case s @ Success(title) =>
          event(Status.Success, s.duration)
          log(_.info, title)
        case UnexpectedFailure(title, t) =>
          event(Status.Error, Duration.Zero)
          log(_.error, title)
          logLevel(level + 1)(_.error, t.getMessage)
          logFor(loggers)(_.trace, t)
        case Failure(title, message) =>
          event(Status.Failure, Duration.Zero)
          log(_.error, title)
          logLevel(level + 1)(_.error, message)
        case Pending(title, message) =>
          event(Status.Pending, Duration.Zero)
          log(_.warn, title + " - " + message)
      }
    }

    report(results, 0)
  }

  private def eventFor(taskDef: TaskDef, eventHandler: EventHandler)(actualStatus: Status, actualDuration: FiniteDuration) =
    eventHandler.handle(
      new Event {
        val duration = actualDuration.toMillis
        val fingerprint = taskDef.fingerprint
        val fullyQualifiedName = taskDef.fullyQualifiedName
        val selector = taskDef.selectors.head
        val status: Status = actualStatus
        val throwable = new OptionalThrowable
      })

  private def logStringFor(loggers: Seq[Logger])(level: Int)(method: Logger => String => Unit, message: String) = {
    val levelIndentation = "  " * level
    val levelMessage =
      message
        .split("(\r\n|\r|\n)")
        .mkString(levelIndentation, "\n" + levelIndentation, "")

    logFor(loggers)(method, levelMessage)
  }

  private def logFor[T](loggers: Seq[Logger])(method: Logger => T => Unit, message: T) = {
    loggers.foreach { logger =>
      method(logger)(message)
    }
  }
}