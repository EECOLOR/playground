package org.qirx.littlespec.sbt

import sbt.testing.EventHandler
import sbt.testing.Logger
import org.qirx.littlespec.Specification
import org.qirx.littlespec.runner.SpecificationRunner

case class Task[T <: Specification](
  testClass: Class[T],
  isObject: Boolean,
  taskDef: sbt.testing.TaskDef,
  runner: SpecificationRunner,
  reporter: SbtReporter) extends sbt.testing.Task {

  val tags: Array[String] = Array.empty

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[sbt.testing.Task] = {

    val testInstance =
      if (isObject) testClass.getField("MODULE$").get(null).asInstanceOf[T]
      else testClass.newInstance

    val results = runner.run(testInstance)

    reporter.report(taskDef, eventHandler, loggers, results)

    Array.empty
  }

}