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

    "report nothing if there are no results" - {
      val (events, logs) = report()

      events is Seq.empty
      logs is Seq.empty
    }

    "report the correct information" - {
      val (events, _) = report(Success("")(1.second))

      events is Seq(Event(Status.Success, 1000))
    }

    "report success" - {

      val (events, logs) = report(Success("test")(1.second))

      events is Seq(Event(Status.Success, 1000))

      logs is Seq(infoLog("test"))

    }

    "report failure" - {
      val (events, logs) = report(Failure("test", "message"))

      events is Seq(Event(Status.Failure))

      logs is Seq(
        failureLog("test"),
        failureLog("  message"))
    }

    "report unexpected failure" - {
      val (events, logs) = report(UnexpectedFailure("test", new Exception("message")))

      events is Seq(Event(Status.Error))

      logs is Seq(
        errorLog("test"),
        errorLog("  message"),
        "trace" -> "[suppressed]"
      )
    }

    "report pending" - {
      val (events, logs) = report(Pending("test", "message"))

      events is Seq(Event(Status.Pending))

      logs is Seq("warn" -> "test - message")
    }

    "report nested" - {
      val (events, logs) = report(CompoundResult("test", Seq.empty))

      events is Seq.empty

      logs is Seq(infoLog("test"))
    }

    "report with indentation" - {
      val (_, logs) = report(CompoundResult("outer", Seq(Success("inner")(1.second))))

      logs is Seq(
        infoLog("outer"),
        infoLog("  inner"))
    }

    "report multiline correctly for different systems" - {
      val (_, logs) = report(CompoundResult("outer",
        Seq(Success("inner1\ninner2\r\ninner3\rinner4")(1.second))))

      logs is Seq(
        infoLog("outer"),
        infoLog("  inner1\n  inner2\n  inner3\n  inner4"))
    }
  }

  private def errorLog(message: String) =
    "error" -> message

  private def infoLog(message: String) =
    "info" -> message

  private def failureLog(message: String) =
    "error" -> message

  def report(in: Result*): (Seq[Event], Seq[(String, String)]) = {
    val out = new HandlerAndLogger
    reporter.report(taskDef, out, Array(out), in)
    (out.events, out.logs)
  }

  private val taskDef = TaskDefFactory.create("test")

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

  class HandlerAndLogger extends EventHandler with Logger {
    var events = Seq.empty[Event]
    var logs = Seq.empty[(String, String)]

    def handle(event: sbt.testing.Event): Unit =
      events :+= Event(event)

    val ansiCodesSupported = true

    def debug(message: String): Unit = logs :+= "debug" -> message
    def error(message: String): Unit = logs :+= "error" -> message
    def info(message: String): Unit = logs :+= "info" -> message
    def warn(message: String): Unit = logs :+= "warn" -> message
    def trace(message: Throwable): Unit = logs :+= "trace" -> "[suppressed]"

  }
}