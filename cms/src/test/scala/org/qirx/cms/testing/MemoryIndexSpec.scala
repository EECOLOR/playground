package org.qirx.cms.testing

import org.qirx.littlespec.Specification
import testUtils.PrettyPrint

class MemoryIndexSpec extends Specification {
  
  "#The memory index should" - {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    
    val indexTester = new IndexTester[PrettyPrint]

    val result = indexTester.test(new MemoryIndex)

    result.foreach {
      case (description, result) =>
        result.fold(
          onSuccess = createFragment(description, success),
          onFailure = {
            case testFailure @ TestFailure(value, expectedValue) =>
              val prettyPrint = testFailure.typeclass
              val prettyValue = prettyPrint print value
              val prettyExpectedValue = prettyPrint print expectedValue

              val failureDescription =
                s"""|Expected:
                    |$prettyExpectedValue
                    |Got:
                    |$prettyValue""".stripMargin

              createFragment(description, failure(failureDescription))
          }
        )
    }

    success
  }
}