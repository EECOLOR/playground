// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec.sbt

import org.qirx.littlespec.Specification
import sbt.testing.Logger
import sbt.testing.EventHandler
import sbt.testing.Event
import sbt.testing.Status
import sbt.testing.TaskDef
import scala.concurrent.duration._
import sbt.testing.OptionalThrowable
import org.qirx.littlespec.Pending
import org.qirx.littlespec.CompoundResult
import org.qirx.littlespec.Failure
import org.qirx.littlespec.Success
import org.qirx.littlespec.Result
import org.qirx.littlespec.UnexpectedFailure
import sbt.testing.Fingerprint
import sbt.testing.Selector

object SbtReporterSpec extends Specification {

  val reporter = new DefaultSbtReporter

  "DefaultSbtReporter should" - {

    "report" - {

      "nothing if there are no results" - {
        val (events, logs) = report()

        events is Seq.empty
        logs is Seq.empty
      }

      "the correct information" - {
        val (events, _) = report(successResult(""))

        events is Seq(Event(Status.Success, 1000))
      }

      "success" - {

        val (events, logs) = report(successResult("test"))

        events is Seq(Event(Status.Success, 1000))

        logs is Seq(
          infoLog(s" $successIndicator test"),
          emptyLine)

      }

      "failure" - {
        val (events, logs) = report(Failure("test", "message"))

        events is Seq(Event(Status.Failure))

        logs is Seq(
          errorLog(s"$failureIndicator test"),
          errorLog(s"    message"),
          emptyLine)
      }

      "unexpected failure" - {
        val (events, logs) = report(UnexpectedFailure("test", new Exception("message")))

        events is Seq(Event(Status.Error))

        logs is Seq(
          errorLog(s"$failureIndicator test"),
          errorLog(s"    message"),
          "trace" -> "[suppressed]",
          emptyLine
        )
      }

      "pending" - {
        val (events, logs) = report(Pending("test", "message"))

        events is Seq(Event(Status.Pending))

        val coloredMessage = warnColor + "message" + resetColor

        logs is Seq(
          "warn" -> s" $pendingIndicator test - $coloredMessage",
          emptyLine)
      }

      "nested" - {
        val (events, logs) = report(CompoundResult("test", Seq.empty))

        events is Seq.empty

        logs is Seq(
          infoLog(" test"),
          emptyLine)
      }

      "nested with indentation" - {
        val (_, logs) = report(CompoundResult("outer", Seq(successResult("inner"))))

        logs is Seq(
          infoLog(s" outer"),
          infoLog(s"   $successIndicator inner"),
          emptyLine)
      }

      "nested with extra nesting" - {
        val (_, logs) = report(CompoundResult("outer", Seq(CompoundResult("inner", Seq(successResult("inner"), Failure("inner", "message"))))))

        logs is Seq(
          infoLog(s" outer"),
          infoLog(s"   - inner"),
          infoLog(s"     $successIndicator inner"),
          errorLog(s"    $failureIndicator inner"),
          errorLog(s"        message"),
          emptyLine)
      }

      "multiline correctly for different systems" - {
        val (_, logs) = report(CompoundResult("outer",
          Seq(successResult("inner1\ninner2\r\ninner3\rinner4"))))

        logs is Seq(
          infoLog(s" outer"),
          infoLog(s"   $successIndicator inner1\n     inner2\n     inner3\n     inner4"),
          emptyLine)
      }
    }

    "strip ansi color codes if the logger does not support it" - {
      val out = new HandlerAndLogger(ansiCodesSupported = false)
      reporter.report(taskDef, out, Array(out), Seq(successResult("test")))
      out.logs is Seq(
          infoLog(" + test"),
          emptyLine)
    }
  }

  val errorColor = "\u001b[31m"
  val successColor = "\u001b[32m"
  val infoColor = "\u001b[36m"
  val warnColor = "\u001b[33m"

  val resetColor = "\u001b[0m"

  val pendingIndicator = warnColor + "o" + resetColor
  val successIndicator = successColor + "+" + resetColor
  val failureIndicator = errorColor + "X" + resetColor

  def successResult(message:String) = Success(message)(1.second)

  def errorLog(message: String) =
    "error" -> message

  def infoLog(message: String) =
    "info" -> message

  def report(in: Result*): (Seq[Event], Seq[(String, String)]) = {
    val out = new HandlerAndLogger
    reporter.report(taskDef, out, Array(out), in)
    (out.events, out.logs)
  }

  val emptyLine = infoLog("")

  val taskDef = TaskDefFactory.create("test")

  case class Event(
    status: Status,
    duration: Long,
    fullyQualifiedName: String,
    fingerprint: Fingerprint,
    selector: Selector,
    throwable: OptionalThrowable)

  object Event {
    def apply(event: sbt.testing.Event): Event =
      Event(event.status, event.duration, event.fullyQualifiedName, event.fingerprint, event.selector, event.throwable)

    def apply(status: Status, duration: Long = 0, throwable: OptionalThrowable = new OptionalThrowable): Event =
      apply(status, duration, taskDef.fullyQualifiedName, taskDef.fingerprint, taskDef.selectors.head, throwable)
  }

  class HandlerAndLogger(val ansiCodesSupported:Boolean = true) extends EventHandler with Logger {
    var events = Seq.empty[Event]
    var logs = Seq.empty[(String, String)]

    def handle(event: sbt.testing.Event): Unit =
      events :+= Event(event)

    def debug(message: String): Unit = logs :+= "debug" -> message
    def error(message: String): Unit = logs :+= "error" -> message
    def info(message: String): Unit = logs :+= "info" -> message
    def warn(message: String): Unit = logs :+= "warn" -> message
    def trace(message: Throwable): Unit = logs :+= "trace" -> "[suppressed]"

  }
}