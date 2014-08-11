package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.testing.StoreTester
import org.qirx.cms.testing.MemoryStore
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.qirx.cms.testing.TestFailure

trait PrettyPrint[T] {
  def print(t: T): String
}
object PrettyPrint {
  implicit val forJsObject = new PrettyPrint[JsObject] {
    def print(o: JsObject) = Json.prettyPrint(o)
  }
}

class _06_01_Memory extends Specification {

  "#The memory store" - {

    val storeTester = new StoreTester[PrettyPrint]

    val result = storeTester.test(MemoryStore)

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