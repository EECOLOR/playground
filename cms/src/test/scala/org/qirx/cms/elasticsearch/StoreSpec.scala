package org.qirx.cms.elasticsearch

import org.qirx.cms.testing.StoreTester
import org.qirx.cms.testing.TestFailure
import org.qirx.littlespec.Specification

import play.api.libs.ws.WS
import play.api.test.FakeApplication
import play.api.test.Helpers
import testUtils.PrettyPrint

class StoreSpec extends Specification {

  "#The ElasticSearch store should" - Helpers.running(FakeApplication()) {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    import play.api.Play.current

    val storeTester = new StoreTester[PrettyPrint]

    val endpoint = "http://localhost:9200"

    val result = storeTester.test(new Store(endpoint, "test_store", WS.client))

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