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
      val log = logLevel(level, true)
      val logError = logLevel(level, false)

      results.foreach {
        case CompoundResult(title, results) =>
          val indicator = if (level == 0) noIndicator else compoundIndicator
          log(_.info, title, indicator)
          report(results, level + 1)

        case s @ Success(title) =>
          event(Status.Success, s.duration)
          log(_.info, title, successIndicator)

        case UnexpectedFailure(title, throwable) =>
          event(Status.Error, Duration.Zero)
          logError(_.error, title, failureIndicator)
          logLevel(level + 2, false)(_.error, throwable.getMessage, noIndicator)
          logFor(loggers)(_.trace, throwable)

        case Failure(title, message) =>
          event(Status.Failure, Duration.Zero)
          logError(_.error, title, failureIndicator)
          logLevel(level + 2, false)(_.error, message, noIndicator)

        case Pending(title, message) =>
          event(Status.Pending, Duration.Zero)
          val coloredMessage = warningColor + message + resetColor
          log(_.warn, title + " - " + coloredMessage, pendingIndicator)
      }
    }

    report(results, 0)
    if (results.nonEmpty) logEmptyLine(loggers)
  }

  private val errorColor = "\u001b[31m"
  private val successColor = "\u001b[32m"
  private val warningColor = "\u001b[33m"
  private val resetColor = "\u001b[0m"

  private val noIndicator = None
  private val successIndicator = Some(successColor + "+" + resetColor)
  private val pendingIndicator = Some(warningColor + "o" + resetColor)
  private val failureIndicator = Some(errorColor + "X" + resetColor)
  private val compoundIndicator = Some("-")

  private def logEmptyLine(loggers: Seq[Logger]) =
    logStringFor(loggers)(level = 0, extraSpace = false)(_.info, "", noIndicator)

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

  private def logStringFor(loggers: Seq[Logger])(level: Int, extraSpace: Boolean)(method: Logger => String => Unit, message: String, indicator: Option[String]) = {
    val (indicatorWithSeparator, indicatorIndentation) =
      indicator.map(_ + " " -> "  ").getOrElse("" -> "")
    val levelIndentation = "  " * level
    val compensation = if (extraSpace) " " else ""
    val levelMessage =
      message
        .split("(\r\n|\r|\n)")
        .mkString(
          start = levelIndentation + compensation + indicatorWithSeparator,
          sep = "\n" + levelIndentation + compensation + indicatorIndentation,
          end = "")

    logFor(loggers, stringColorRemover)(method, levelMessage)
  }

  private def logFor[T](loggers: Seq[Logger], colorRemover: T => T = identity[T] _)(
    method: Logger => T => Unit, message: T) =

    loggers.foreach { logger =>

      val cleanMessage =
        if (logger.ansiCodesSupported) message
        else colorRemover(message)

      method(logger)(cleanMessage)
    }

  private def stringColorRemover(message: String) = {
    val colorPattern = raw"\u001b\[\d{1,2}m"
    message.replaceAll(colorPattern, "")
  }
}