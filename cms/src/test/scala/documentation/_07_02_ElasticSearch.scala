package documentation

import org.qirx.littlespec.Specification
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import org.qirx.cms.testing.TestFailure
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import testUtils.PrettyPrint
import testUtils.codeString
import org.qirx.cms.testing.IndexTester
import play.api.libs.ws.WS
import play.api.Play.current
import org.qirx.cms.elasticsearch
import play.api.test.Helpers
import play.api.test.Helpers.contentAsJson
import play.api.test.Helpers.defaultAwaitTimeout
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
import testUtils.Example
import testUtils.cmsName
import com.ning.http.client.AsyncHttpClientConfig
import play.api.libs.ws.ning.NingWSClient
import play.api.libs.json.JsArray
import play.api.libs.ws.WSClient
import testUtils.TestClient
import testUtils.TestResponse
import play.api.test.FakeRequest
import play.api.libs.ws.InMemoryBody
import play.api.mvc.Request
import play.api.mvc.AnyContent
import play.api.mvc.Result

class _07_02_ElasticSearch extends Specification with Example {

  "#The ElasticSearch index" - {

    val builder = new (AsyncHttpClientConfig.Builder)()
    val client = new NingWSClient(builder.build())

    """|The Elastic Search index requires access to your document metadata because
       |it will create mappings based on that metadata when it's instantiated.
       |
       |The index needs more metadata than is normally available, to make it more 
       |usable you need to create an instance of `elasticsearch.Document` instead
       |of the normal `Document`.
       |
       |This `elasticsearch.Document` requires the properties to have extra index
       |information. That information can be automatically added when you import 
       |the correct implicit conversion.""".stripMargin -
      new ExampleContainer {
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

        val index = new elasticsearch.Index(documents, endpoint, indexName, client)
      }.withSpecification { body =>
        val documents = body.documents
        val index = body.index
        val indexName = body.indexName
        val endpoint = body.endpoint

        "The index has created a mapping using the information in the metadata" - example {
          val response = client.url(s"$endpoint/$indexName/_mapping").get.map(_.json)

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
                    "body_text" -> obj(
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
        }

        """|Note that the an extra field has been introduced for `body` called 
           |`body_text`. This field contains the text as a string instead of 
           |the rich text structure. This can be used to perform searches.
           |
           |Below an example of a search that would fail if we did not have 
           |this field. As a bonus it can be used for highlighting as well.""".stripMargin - example {

          val article = obj(
            "title" -> "Test article",
            "body" -> arr(
              obj(
                "element" -> "p",
                "children" -> arr(
                  "This ",
                  obj(
                    "element" -> "em",
                    "text" -> "article"
                  ),
                  " is about special things"
                )
              )
            )
          )

          val articleInIndex = index(Index.Put("article", "test_id", article))
          Await.result(articleInIndex, 1.second)

          val queryWithHighlight =
            obj(
              "query" -> obj(
                "match" -> obj(
                  "body_text" -> obj("query" -> "\"this article\"")
                )
              ),
              "highlight" -> obj(
                "fields" -> obj("body_text" -> obj())
              )
            )

          val response =
            client
              .url(s"$endpoint/$indexName/_search")
              .withBody(queryWithHighlight)
              .get

          val result = Await.result(response, 1.second)

          val source = (result.json \\ "_source").head
          source is obj(
            "title" -> "Test article",
            "body" -> arr(
              obj(
                "element" -> "p",
                "children" -> arr(
                  "This ",
                  obj(
                    "element" -> "em",
                    "text" -> "article"
                  ),
                  " is about special things"
                )
              )
            ),
            "body_text" -> "This article is about special things"
          )

          val highlight = (result.json \\ "highlight").head
          highlight is obj(
            "body_text" -> arr("<em>This</em> <em>article</em> is about special things")
          )
        }

        "Finally we need to close the client" - {
          client.close()
          success
        }

        class ProxyCall(fakeResponse: JsObject, f: (Request[AnyContent], Seq[String]) => Index[Result]) {
          val json = codeString {
            fakeResponse
          }

          import org.qirx.cms.elasticsearch

          val testClient = new TestClient(new TestResponse(json = json.value))

          val index = new elasticsearch.Index(documents, endpoint, indexName, testClient)

          val expectedBody = obj("some" -> "body")
          val request = FakeRequest("GET", "/ignored/?some=query").withJsonBody(expectedBody)

          val path = "article"
          val result = contentAsJson(index(f(request, Seq(path))))

          val lastRequestHolder = testClient.lastRequestHolder
          val calledUrl = lastRequestHolder.url
          val receivedQueryString = lastRequestHolder.queryString
          val receivedBody = lastRequestHolder.body
        }

        """|The index provides search handling that will act as a proxy to the 
           |Elastic Search `_search` endpoint.""".stripMargin - {

          val proxyCall = new ProxyCall(
            obj(
              "hits" -> obj(
                "total" -> 1,
                "hits" -> arr(
                  obj("_id" -> "some id")
                )
              )
            ),
            (request, remainingPathSegments) => Index.Search(request, remainingPathSegments))
          import proxyCall._

          s"""|Calling `Search` with a request and `Seq("$path")` as 
              |remaining path segments results in the following call to 
              |Elastic Search:""".stripMargin - example {
            calledUrl is s"$endpoint/$indexName/article/_search"
          }

          s"""|Note that the search method will extract the `"hits"` element
              |from the result.
              |
              |The result from the server:
              |```
              |$json
              |```""".stripMargin - example {
            result is obj(
              "total" -> 1,
              "hits" -> arr(
                obj("_id" -> "some id")
              )
            )
          }

          "The index will forward the query string of the incoming request" - {
            receivedQueryString is Map("some" -> Seq("query"))
          }

          "It will also forward the body" - {
            receivedBody isLike {
              case InMemoryBody(bytes) => Json.parse(bytes) is expectedBody
            }
          }
        }

        """|The index provides count handling that will act as a proxy to the 
           |Elastic Search `_count` endpoint.""".stripMargin - {

          val proxyCall = new ProxyCall(
              obj("count" -> 1, "_shards" -> obj("some" -> "value")),
              (request, remainingPathSegments) => Index.Count(request, remainingPathSegments))
          import proxyCall._

          s"""|Calling `Count` with a request and `Seq("$path")` as 
              |remaining path segments results in the following call to 
              |Elastic Search:""".stripMargin - example {
            calledUrl is s"$endpoint/$indexName/article/_count"
          }

          s"""|The count method will return the `"count"` element from the result.
              |
              |The result from the server:
              |```
              |$json
              |```""".stripMargin - example {
            result is obj(
              "count" -> 1
            )
          }

          "The index will forward the query string of the incoming request" - {
            receivedQueryString is Map("some" -> Seq("query"))
          }

          "It will also forward the body" - {
            receivedBody isLike {
              case InMemoryBody(bytes) => Json.parse(bytes) is expectedBody
            }
          }
        }
      }

    "test different mappings (json)" - {}

    s"""|The following tests ensure it acts as expected from the persfective of 
        |the $cmsName.
        |
        |The index should""".stripMargin - runningFakeApplication {

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

  }

  def runningFakeApplication[T](block: => T): T = Helpers.running(FakeApplication())(block)
}