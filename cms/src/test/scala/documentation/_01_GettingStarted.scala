package documentation

import org.qirx.littlespec.Specification
import testUtils.Example
import testUtils.cmsName
import org.qirx.cms.Cms
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import play.api.libs.json.JsValue
import scala.language.reflectiveCalls
import play.api.test.Helpers
import play.api.test.FakeRequest
import play.api.test.FakeApplication
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.util.Timeout
import play.api.Application
import play.api.mvc.Request
import play.api.http.Writeable
import org.qirx.littlespec.reporter.MarkdownReporter
import testUtils.GetFromApplication
import testUtils.PostToApplication
import scala.reflect.ClassTag
import testUtils.TestApplication
import testUtils.TestStore
import testUtils.TestEnvironment
import testUtils.withFixedDateTime


object _01_GettingStarted extends Specification with Example {

  s"""|#Getting started
      |
      |The first thing you need to do is to create an instance of `$cmsName`  
      |and provide the information it needs to operate.""".stripMargin -
    new ExampleContainer {
      import org.qirx.cms.Cms
      import org.qirx.cms.metadata.dsl._
      import org.qirx.cms.metadata.properties._
      import scala.concurrent.Future
      import play.api.mvc.RequestHeader

      def customAuthenticate(requestHeader: RequestHeader): Future[Boolean] =
        Future.successful {
          requestHeader.headers
            .get("X-Qirx-Authenticate")
            .filter(_ == "let me in")
            .isDefined
        }

      val cms = new Cms(
        pathPrefix = "/api",
        authenticate = customAuthenticate,
        environment = new TestEnvironment,
        documents = Seq(
          Document(id = "article", idField = "title")(
            "title" -> Label,
            "secret" -> Confidential(Label.?),
            "body" -> RichContent.?,
            "tags" -> Tag.*,
            "date" -> Date.generated,
            "publishDate" -> Date.? 
          )
        )
      )
    }
    .text(
      s"""|The `$cmsName` has a single method to handle the requests,
          |this method will automatically select the appropriate action. 
          |Below an example of it within GlobalSettings.""".stripMargin)
    .code { body =>
      val cms = body.cms

      new ExampleContainer {
        import play.api.GlobalSettings
        import play.api.mvc.Handler
        import play.api.mvc.RequestHeader

        object CustomGlobal extends GlobalSettings {

          override def onRouteRequest(request: RequestHeader): Option[Handler] =
            cms.handle(request, orElse = super.onRouteRequest)
        }
      }
    }
    .withSpecification { body =>

      val app = TestApplication.fakeApplication(Some(body.CustomGlobal))

      val POST = new PostToApplication(app)
      val GET = new GetFromApplication(app)

      """|This gives you an API that consists of three parts:
         | - *private* - Allows you to manage documents
         | - *public* - Allows you to retrieve and search documents
         | - *metadata* - Provides the metadata you specified
         |
         |## The private API""".stripMargin - {

        s"""|${moreInformation[_02_Private_Api]}
            |
            |This part of the API allows you to change content, that's the 
            |reason this requires authentication. Note that we have specified 
            |the authentation mechanism when we created the `$cmsName`.""".stripMargin - {
          example {
            val article = obj("title" -> "Article 1")
            val auth = "X-Qirx-Authenticate" -> "let me in"

            val (status, body) = withFixedDateTime {
              POST(article) withHeader auth to "/api/private/article"
            } 

            body is obj(
              "id" -> "article_1"
            )
            status is 201
          }

          val expectedResult = codeString {
            obj(
              "status" -> 403,
              "error" -> "forbidden"
            )
          }

          s"""|Failing to authicate results in a response like this:
              |```scala
              |$expectedResult          
              |```""".stripMargin - {

            val article = obj("title" -> "Article 1")

            val (status, body) = POST(article) to "/api/private/article"

            status is 403
            body is expectedResult.value
          }
        }

      }
      "## The public API" - {

        s"""|${moreInformation[_03_Public_Api]}
            |
            |This part of the API allows you to search and retrieve content, 
            |it does not require authentication.""".stripMargin - {
          example {
            val (status, body) = GET from "/api/public/article"

            status is 200
            body is arr(
              obj(
                "id" -> "article_1",
                "title" -> "Article 1",
                "date" -> "2011-07-10T20:39:21+02:00"
              )
            )
          }
        }
      }
      "## The metada API" - {

        s"""|${moreInformation[_04_MetadataApi]}
            |
            |This part of the API allows you to retrieve the metadata of documents,
            |it does not require authentication and is read-only.""".stripMargin - {

          example {
            val (status, body) = GET from "/api/metadata/article"

            status is 200
            body is arr(
              obj(
                "id" -> "article",
                "properties" -> arr(
                  obj(
                    "id" -> "label",
                    "name" -> "title"
                  ),
                  obj(
                    "id" -> "label",
                    "name" -> "secret"
                  ),
                  obj(
                    "id" -> "rich_content",
                    "name" -> "body",
                    "optional" -> true,
                    "extra" -> obj(
                      "allowedElements" -> arr(
                        "strong", "em", "ul", "ol", "li", "span[class|lang]",
                        "a[href|hreflang|title|target]", "br", "p[class|lang]")
                    )
                  ),
                  obj(
                    "id" -> "tag",
                    "name" -> "tags",
                    "set" -> true,
                    "nonEmpty" -> false
                  ),
                  obj(
                    "id" -> "date",
                    "name" -> "date",
                    "generated" -> true
                  )
                )
              )
            )
          }
        }

      }
    }
}
