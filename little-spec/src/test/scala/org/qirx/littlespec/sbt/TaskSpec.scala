// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec.sbt

import scala.language.existentials
import org.qirx.littlespec.Specification
import org.qirx.littlespec.runner.SpecificationRunner
import sbt.testing.Event
import sbt.testing.EventHandler
import sbt.testing.Logger
import sbt.testing.TaskDef
import org.qirx.littlespec.fragments.Success
import org.qirx.littlespec.fragments.Result
import scala.concurrent.duration._
import org.qirx.littlespec.fragments.Result
import org.qirx.littlespec.fragments.Text
import testUtils.TaskDefFactory

object TaskSpec extends Specification {

  "Task should" - {

    "return no tasks" - {

      val task = createTask(noOpRunner, noOpReporter)

      val tasks = task.execute(noOpEventHandler, Array(noOpLogger))

      tasks.size is 0
    }

    "run the correct specification" - {

      def testRunSpecification(forObject: Boolean) = {

        val actualTestClass = if (forObject) testObject else testClass

        var specificationName: Option[String] = None

        val stubRunner =
          new SpecificationRunner {
            def run(specification: Specification) = {
              specificationName = Some(specification.getClass.getName)
              Seq.empty
            }
          }

        val task = createTask(stubRunner, noOpReporter, forObject)

        val tasks = task.execute(noOpEventHandler, Array(noOpLogger))

        specificationName is Some(actualTestClass.getName)
      }

      "for classes" - testRunSpecification(forObject = false)

      "for objects" - testRunSpecification(forObject = true)
    }

    "report the results correctly" - {

      val results = Seq(Success(Text("test"))(1.second))
      var reportCalls: Seq[(TaskDef, EventHandler, Seq[Logger], Seq[Result])] = Seq.empty

      val stubRunner =
        new SpecificationRunner {
          def run(specification: Specification) = results
        }

      val mockReporter =
        new SbtReporter {
          def report(taskDef: TaskDef, eventHandler: EventHandler, loggers: Seq[Logger], results: Seq[Result]): Unit =
            reportCalls :+= (taskDef, eventHandler, loggers, results)
        }

      val task = createTask(stubRunner, mockReporter)

      val loggers = Array(noOpLogger)

      task.execute(noOpEventHandler, loggers)

      val expectedReportCall = (task.taskDef, noOpEventHandler, loggers.toSeq, results)
      reportCalls is Seq(expectedReportCall)
    }
  }

  val testClass = classOf[EmptyTestSpecificationClass]
  val testObject = EmptyTestSpecificationObject.getClass

  def createTask(runner: SpecificationRunner, reporter: SbtReporter, forObject: Boolean = false) = {

    val clazz =
      if (forObject) testObject
      else testClass

    val taskDef = TaskDefFactory.create(clazz.getName, forObject)

    Task(clazz, forObject, taskDef, runner, reporter)
  }

  val noOpRunner =
    new SpecificationRunner {
      def run(specification: Specification) = Seq.empty
    }

  val noOpReporter =
    new SbtReporter {
      def report(taskDef: TaskDef, eventHandler: EventHandler, loggers: Seq[Logger], results: Seq[Result]): Unit =
        ()
    }

  val noOpEventHandler =
    new EventHandler {
      def handle(e: Event): Unit = ???
    }

  val noOpLogger =
    new Logger {
      def ansiCodesSupported(): Boolean = ???
      def debug(message: String): Unit = ???
      def error(message: String): Unit = ???
      def info(message: String): Unit = ???
      def trace(throwable: Throwable): Unit = ???
      def warn(message: String): Unit = ???
    }

}

class EmptyTestSpecificationClass extends Specification
object EmptyTestSpecificationObject extends Specification