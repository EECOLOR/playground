package org.qirx.littlespec.sbt

import sbt.testing.Fingerprint
import java.net.URLClassLoader
import java.io.File
import org.qirx.littlespec.Specification
import testUtils.TaskDefFactory

class RunnerSpec extends Specification {

  val location = new File("little-spec/testClasses")
  val testClassLoader = new URLClassLoader(Array(location.toURI.toURL), getClass.getClassLoader)
  def newRunner = new Runner(Array.empty, Array.empty, testClassLoader)

  "The Runner" - {

    "should return an empty string when done is called" - {
      newRunner.done is ""
    }

    "should throw an illegal state exception when tasks is called after done" - {
      val runner = newRunner
      runner.done
      runner.tasks(Array.empty) must throwAn[IllegalStateException].like { e =>
        e.getMessage contains "done" is true
        e.getMessage contains "tasks" is true
      }
    }

    "should return the correct tasks" - {

      def testTaskCreation(testClassName: String, isObject: Boolean) = {
        var actualTestClassName = testClassName
        if (isObject) actualTestClassName += "$"
        val taskDef = TaskDefFactory.create(testClassName, isObject)

        val testClass = testClassLoader.loadClass(actualTestClassName)
          .asSubclass(classOf[Specification])

        val tasks = newRunner.tasks(Array(taskDef))

        tasks isLike {
          case Array(task: Task[_]) =>
            task.isObject is isObject
            task.taskDef is taskDef
            task.testClass is testClass
        }
      }
      "for objects" - testTaskCreation("test.EmptyObject", isObject = true)
      "for classes" - testTaskCreation("test.EmptyClass", isObject = false)
    }
  }
}