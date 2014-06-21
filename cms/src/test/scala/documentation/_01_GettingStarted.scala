package documentation

import org.qirx.littlespec.Specification
import testUtils.Example
import org.qirx.cms.Cms
import play.api.libs.json.Json.obj
import play.api.libs.json.JsValue
import scala.language.reflectiveCalls
import play.api.test.Helpers
import play.api.test.FakeRequest
import play.api.test.FakeApplication
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.util.Timeout

object _01_GettingStarted extends Specification with Example {

  val cmsClassName = classOf[Cms].getSimpleName

  s"""|#Getting started
      |
      |The first thing you need to do is to create an instance of `$cmsClassName`  
      |and provide the information it needs to operate.""".stripMargin -
    new ExampleContainer {
      import org.qirx.cms.Cms
      import org.qirx.cms.metadata.dsl._
      import org.qirx.cms.metadata.properties._
      import scala.concurrent.Future
      import play.api.mvc.RequestHeader

      def customAuthentication(requestHeader: RequestHeader): Future[Boolean] =
        Future.successful {
          requestHeader.headers
            .get("X-Qirx-Authenticate")
            .filter(_ == "let me in")
            .isDefined
        }

      val cms = new Cms(
        pathPrefix = "/api",
        authentication = customAuthentication,
        documents = Seq(
          Document(id = "article")(
            "title" -> Label,
            "body" -> RichContent.?,
            "tags" -> Tag.*,
            "date" -> Date.generated
          )
        )
      )
    }
    .text(
      s"""|The `$cmsClassName` has a single method to handle the requests,
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
      """|This gives you an API that consists of three parts:
         | - *private* - Allows you to manage documents
         | - *public* - Allows you to retrieve and search documents
         | - *metadata* - Provides the metadata you specified
         |
         |## The private API""".stripMargin - {

        val app = new FakeApplication(withGlobal = Some(body.CustomGlobal))

        object POST {
          class WithTo(body: JsValue, header: Option[(String, String)] = None) {

            def to(path: String) = {
              val request = FakeRequest("POST", path)
                .withHeaders(header.toSeq: _*)
                .withBody(body)
              val result = Helpers.running(app) {
                Helpers.route(app, request).get
              }
              implicit val timeout = Timeout(1.second)
              (Helpers.status(result), Helpers.contentAsJson(result))
            }
          }

          def apply(json: JsValue) = new WithTo(json) {
            def withHeader(header: (String, String)) = new WithTo(json, Some(header))
          }
        }

        """|This part of the API allows you to change content, that's the 
           |reason this requires authentication""".stripMargin - {
          example {
            val article = obj("label" -> "Article 1")
            val auth = "X-Qirx-Authenticate" -> "let me in"

            val (status, body) = POST(article) withHeader auth to "/api/private/article"

            status is 201
            body is obj(
              "id" -> "article_1"
            )
          }

          val expectedResult = codeString {
            obj(
              "status" -> 403,
              "error" -> "accessDenied"
            )
          }

          s"""|Failing to authicate results in a response like this:
              |```scala
              |$expectedResult          
              |```""".stripMargin - {

            val article = obj("label" -> "Article 1")

            val (status, body) = POST(article) to "/api/private/article"

            status is 403
            body is expectedResult.value
          }
        }
      }
    }

  /*{ body =>
      
    }
    * 
    */
}