package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.testing.MemoryIndex
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import testUtils.PrettyPrint
import org.qirx.cms.testing.IndexTester

class _07_01_Memory extends Specification {

  "#The memory index should" - {

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