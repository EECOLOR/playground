package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.testing.StoreTester
import org.qirx.cms.testing.MemoryStore
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import testUtils.PrettyPrint

class _06_01_Memory extends Specification {

  "#The memory store should" - {

    val storeTester = new StoreTester[PrettyPrint]

    val result = storeTester.test(new MemoryStore)

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