package documentation

import org.qirx.littlespec.Specification
import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import testUtils.RouteRequest
import testUtils.testCmsMetadataJson
import play.api.test.Helpers
import play.api.test.FakeRequest

class _04_MetadataApi extends Specification with ApiExampleSpecification {

  "# The metadata API" - {

    val metadaApiPrefix = "/api/metadata"

    withApiExampleIntroduction(apiPrefix = metadaApiPrefix) { app =>

      val GET = new GetFromApplication(app, metadaApiPrefix)

      """|Note that this API only supports GET requests, it will return a failure
         |for any other method""".stripMargin - {
        val helper =
          new RouteRequest {
            val application = app
          }
        import helper._
        import Helpers._
        example {
          val methods = Seq("HEAD", "POST", "PUT", "DELETE", "TRACE", "OPTIONS", "CONNECT", "PATCH")
          methods.foreach { method =>
            val (status, body) = routeRequest(FakeRequest(method, "/api/metadata"))
            body is obj(
              "status" -> 405,
              "error" -> "methodNotAllowed"
            )
            status is 405
          }
          success
        }
      }

      """|The metadata API is a read only API, metadata can only be changed at
         |compile time. This is by design, the compiler does a lot of complicated
         |validation that would otherwise be tricky to implement. On top of that,
         |it's nice to see the metadata evolve alongside with your code. Modern 
         |version control is a great help in tracking and managing changes. 
         |
         |You can get the metadata for all documents like this:""".stripMargin - exampleWithReplacements {
        val (status, body) = GET from "/documents"

        status is 200
        body is arr(
          testCmsMetadataJson.value
        )
      }

      "It's also possible to retrieve the metadata for a single document" - exampleWithReplacements {
        val (status, body) = GET from "/documents/article"

        status is 200
        body is testCmsMetadataJson.value
      }
    }
  }
}