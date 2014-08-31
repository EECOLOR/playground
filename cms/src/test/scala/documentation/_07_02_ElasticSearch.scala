package documentation

import org.qirx.littlespec.Specification
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import testUtils.PrettyPrint
import org.qirx.cms.testing.IndexTester
import play.api.libs.ws.WS
import play.api.Play.current
import org.qirx.cms.elasticsearch.ElasticSearchIndex
import play.api.test.Helpers
import play.api.test.FakeApplication

class _07_02_ElasticSearch extends Specification {

  "#The ElasticSearch index should" - Helpers.running(FakeApplication()) {

    val indexTester = new IndexTester[PrettyPrint]

    val endpoint = "http://localhost:9200"
    
    val result = indexTester.test(new ElasticSearchIndex(endpoint, "test_index", WS.client))

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