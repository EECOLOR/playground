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
import java.net.URI
import play.api.libs.ws.WS
import play.api.Play.current
import play.api.test.Helpers
import play.api.test.FakeApplication
import org.qirx.cms.elasticsearch.ElasticSearchStore

class _06_02_ElasticSearch extends Specification {

  "#The ElasticSearch store should" - Helpers.running(FakeApplication()) {

    val storeTester = new StoreTester[PrettyPrint]

    val endpoint = "http://localhost:9200"
    
    val result = storeTester.test(new ElasticSearchStore(endpoint, "test_store", WS.client))

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