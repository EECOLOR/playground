package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.testing.StoreTester
import org.qirx.cms.testing.MemoryStore
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue

trait PrettyPrint[T] {
  def print(t: T): String
}
object PrettyPrint {
  def apply[T](f:T => String) = new PrettyPrint[T] {
    def print(t:T) = f(t)
  }
  
  implicit def forBoolean = PrettyPrint[Boolean](_.toString)
  
  implicit def forString = PrettyPrint[String]("\"" + _ + "\"")
  
  implicit def forJsValue = PrettyPrint[JsObject](Json.prettyPrint)
  
  implicit def forSeq[T](implicit pretty: PrettyPrint[T]) =
    PrettyPrint[Seq[T]] { _.map(pretty print _).toString }

  implicit def forOpt[T](implicit pretty: PrettyPrint[T]) = 
    PrettyPrint[Option[T]] { _.map(pretty print _).toString } 
  
  implicit def forMap[T](implicit pretty: PrettyPrint[T]) =
    PrettyPrint[Map[String, T]] {
      _.map {
        case (key, value) => key -> (pretty print value)
      }.toString
    }
}

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