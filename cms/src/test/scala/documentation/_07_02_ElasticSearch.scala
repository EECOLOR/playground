package documentation

import org.qirx.littlespec.Specification
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.obj
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import testUtils.PrettyPrint
import org.qirx.cms.testing.IndexTester
import play.api.libs.ws.WS
import play.api.Play.current
import org.qirx.cms.elasticsearch
import play.api.test.Helpers
import play.api.test.FakeApplication
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.metadata.properties.Date
import org.qirx.cms.metadata.dsl.ConfidentialProperty
import org.qirx.cms.metadata.dsl.Confidential
import org.qirx.cms.metadata.PropertyMetadata
import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.dsl.OptionalValueProperty
import org.qirx.cms.metadata.dsl.ValueSetProperty
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.GeneratedValueProperty
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.construction.Index
import scala.concurrent.Await
import scala.concurrent.duration._

class _07_02_ElasticSearch extends Specification {

  "#The ElasticSearch index" - {

    "should" - Helpers.running(FakeApplication()) {

      val indexTester = new IndexTester[PrettyPrint]

      val endpoint = "http://localhost:9200"

      val result = indexTester.test(new elasticsearch.Index(Seq.empty, endpoint, "test_index", WS.client))

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

    "example usage" -
      example {
        Helpers.running(FakeApplication()) {
          import org.qirx.cms.elasticsearch
          import elasticsearch.Document.Implicits.propertyWithIndexInfo

          val documents = Seq(
            elasticsearch.Document(id = "article", idField = "title")(
              "title" -> Label,
              "secret" -> Confidential(Label.?),
              "body" -> RichContent.?,
              "tags" -> Tag.*,
              "date" -> Date.generated,
              "publishDate" -> Date.?
            )
          )
          val endpoint = "http://localhost:9200"
          val indexName = "test_example"
          val client = WS.client

          val index = new elasticsearch.Index(documents, endpoint, indexName, client)

          val response =
            client
              .url(endpoint + "/" + indexName + "/_mapping")
              .get
              .map(_.json)

          val result = Await.result(response, 2.seconds)

          result is obj(
            "test_example" -> obj(
              "mappings" -> obj(
                "article" -> obj(
                  "dynamic" -> "strict",
                  "date_detection" -> false,
                  "properties" -> obj(
                    "id" -> obj(
                      "type" -> "string",
                      "index" -> "not_analyzed"
                    ),
                    "title" -> obj(
                      "type" -> "string"
                    ),
                    "body" -> obj(
                      "type" -> "object",
                      "enabled" -> false
                    ),
                    "body.text" -> obj(
                      "type" -> "string"
                    ),
                    "tags" -> obj(
                      "type" -> "string",
                      "index" -> "not_analyzed"
                    ),
                    "date" -> obj(
                      "type" -> "date",
                      "format" -> "date_time_no_millis"
                    ),
                    "publishDate" -> obj(
                      "type" -> "date",
                      "format" -> "date_time_no_millis"
                    )
                  )
                )
              )
            )
          )

          todo
        }
      }

    "test different mappings (json)" - {}
  }
}