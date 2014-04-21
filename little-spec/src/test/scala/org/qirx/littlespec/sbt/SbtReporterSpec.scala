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

object SbtReporterSpec extends Specification {

  val reporter = new DefaultSbtReporter

  "DefaultSbtReporter should" - {

    "report nothing if there are no results" - {
      val in = Seq.empty
      val out = report(in)

      out.events is Seq.empty
      out.logs is Seq.empty
    }

    "report the correct information from the taskDef" - {
      val taskDef = TaskDefFactory.create("test")
      val in = Seq(Success("")(1.second))
      val out = report(in, taskDef)

      val event = out.events.head
      event.fingerprint is taskDef.fingerprint
      event.fullyQualifiedName is taskDef.fullyQualifiedName
      event.selector is taskDef.selectors.head
      event.throwable is new OptionalThrowable()
    }

    "report success" - {
      testResult(
        Success("test")(1.second), Status.Success, 1000, "info", "test")
    }

    "report failure" - {
      val in = Failure("test", "message")
      val out = report(Seq(in))

      out.events.size is 1
      val event = out.events.head
      event.status is Status.Failure
      event.duration is 0

      out.logs.size is 2
      out.logs is Seq(("error", "test"), ("error", "  message"))
    }

    "report unexpected failure" - {
      val in = UnexpectedFailure("test", new Exception("message"))
      val out = report(Seq(in))

      out.events.size is 1
      val event = out.events.head
      event.status is Status.Error
      event.duration is 0

      out.logs.size is 3
      out.logs.take(2) is Seq(("error", "test"), ("error", "  message"))
      out.logs.drop(2).head isLike {
        case ("trace", _) => success
      }
    }

    "report pending" - {
      testResult(
        Pending("test", "message"), Status.Pending, 0, "warn", "test - message")
    }

    "report nested" - {
      val in = CompoundResult("test", Seq.empty)
      val out = report(Seq(in))

      out.events.size is 0

      out.logs.size is 1
      out.logs.head is ("info", "test")
    }

    "report with indentation" - {
      val in = CompoundResult("outer", Seq(Success("inner")(1.second)))
      val out = report(Seq(in))

      out.logs.size is 2
      out.logs is Seq(("info", "outer"), ("info", "  inner"))
    }
  }

  def testResult(in: Result, status: Status, duration: Long, method: String, message: String) = {
    val out = report(Seq(in))

    out.events.size is 1
    val event = out.events.head
    event.status is status
    event.duration is duration

    out.logs.size is 1
    out.logs.head is (method, message)
  }

  def report(in: Seq[Result], taskDef: TaskDef = TaskDefFactory.create("test")): HandlerAndLogger = {
    val out = new HandlerAndLogger
    reporter.report(taskDef, out, Array(out), in)
    out
  }

  class HandlerAndLogger extends EventHandler with Logger {
    var events = Seq.empty[Event]
    var logs = Seq.empty[(String, String)]

    def handle(event: Event): Unit =
      events :+= event

    val ansiCodesSupported = true

    def debug(message: String): Unit = logs :+= "debug" -> message
    def error(message: String): Unit = logs :+= "error" -> message
    def info(message: String): Unit = logs :+= "info" -> message
    def warn(message: String): Unit = logs :+= "warn" -> message
    def trace(message: Throwable): Unit = logs :+= "trace" -> ""

  }
}