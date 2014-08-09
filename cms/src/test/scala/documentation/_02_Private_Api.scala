package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.metadata.dsl.Document
import testUtils.Example
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.metadata.properties.Date
import org.qirx.cms.Cms
import scala.concurrent.Future
import testUtils.cmsName
import testUtils.TestApplication
import testUtils.PostToApplication
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import scala.util.Try
import play.api.i18n.Messages
import play.api.test.Helpers
import scala.io.Source
import play.api.libs.json.Writes
import play.api.libs.json.Reads
import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication
import testUtils.PutToApplication
import testUtils.RouteRequest
import play.api.test.FakeRequest
import testUtils.PatchToApplication

class _02_Private_Api extends Specification with ApiExampleSpecification {

  "# The private API" - {

    val privateApiPrefix = "/api/private"

    withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

      val POST = new PostToApplication(app, privateApiPrefix)
      val GET = new GetFromApplication(app, privateApiPrefix)
      val PUT = new PutToApplication(app, privateApiPrefix)
      val PATCH = new PatchToApplication(app, privateApiPrefix)

      """|Note that this API only supports a few request methods, it will return 
         |a failure for any non-suported method""".stripMargin - {
        val helper =
          new RouteRequest {
            val application = app
          }
        import helper._
        import Helpers._
        example {
          val methods = Seq("HEAD", "DELETE", "TRACE", "OPTIONS", "CONNECT")
          methods.foreach { method =>
            val (status, body) = routeRequest(FakeRequest(method, "/api/private"))
            body is obj(
              "status" -> 405,
              "error" -> "methodNotAllowed"
            )
            status is 405
          }
          success
        }
      }

      "The first call we make to the api returns no documents" - example {
        val (status, body) = GET from "/article"
        status is 200
        body is arr()
      }

      "So we need to create a new instance" - example {
        val article = obj(
          "title" -> "Article 1",
          "secret" -> "Secret information about Article 1",
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
            ),
            obj(
              "element" -> "p",
              "attributes" -> obj("class" -> "quote"),
              "children" -> arr(
                "All articles are special"
              )
            )
          ),
          "tags" -> arr("tag1", "tag2")
        )

        val (status, body) = POST(article) to "/article"

        status is 201
        body is obj(
          "id" -> "article_1"
        )
      }

      "The article can now be retrieved" - {
        val (status, body) = GET from "/article"

        status is 200
        body is arr(
          obj(
            "id" -> "article_1",
            "title" -> "Article 1",
            "secret" -> "Secret information about Article 1",
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
              ),
              obj(
                "element" -> "p",
                "attributes" -> obj("class" -> "quote"),
                "children" -> arr(
                  "All articles are special"
                )
              )
            ),
            "tags" -> arr("tag1", "tag2")
          )
        )
      }

      s"""|There are scenario's where you want to list articles, but you don't
          |want to retrieve all fields.""".stripMargin - example {
        val (status, body) = GET from "/article?fields=id,title"

        status is 200
        body is arr(
          obj(
            "id" -> "article_1",
            "title" -> "Article 1"
          )
        )
      }

      "Let's create another article" - example {
        val article = obj(
          "title" -> "Article 3",
          "tags" -> arr("tag1")
        )

        val (status, body) = POST(article) to "/article"

        status is 201
        body is obj(
          "id" -> "article_3"
        )
      }

      "You can retrieve a single document by specifying it's id" - example {
        val (status, body) = GET from "/article/article_1?fields=id,title"

        status is 200
        body is obj(
          "id" -> "article_1",
          "title" -> "Article 1"
        )
      }

      """|As you may have noticed, I accidentally created an article with 
         |the wrong title, let's fix that""".stripMargin - example {
        val article = obj(
          "title" -> "Article 2"
        )

        val (status, body) = PUT(article) at "/article/article_3"
        status is 204
        body is null
      }

      "Note that the article is overwritten" - {
        val (status, body) = GET from "/article/article_3"
        status is 200
        body is obj(
          "id" -> "article_3",
          "title" -> "Article 2"
        )
      }

      "We could however partially update the article" - example {
        val withTags = obj(
          "tags" -> arr("tag1", "tag3")
        )

        val (status, body) = PATCH("/article/article_3") using withTags 
        status is 204
        body is null
      }

      "As you can see the result is tags being added" - example {
        val (status, body) = GET from "/article/article_3"
        status is 200
        body is obj(
          "id" -> "article_3",
          "title" -> "Article 2",
          "tags" -> arr("tag1", "tag3")
        )

      }

      "It's also possible to update the id." - example {
        val newId = obj("id" -> "article_2")

        PATCH("/article/article_3") using newId 

        val (status, body) = GET from "/article/article_2?fields=id,title"
        status is 200
        body is obj(
          "id" -> "article_2",
          "title" -> "Article 2"
        )
      }

      """|The old id might already be stored somewhere so we can still 
         |retrieve the new document with it's old id.""".stripMargin - example {
        val (status, body) = GET from "/article/article_3?fields=id,title"

        status is 200
        body is obj(
          "id" -> "article_2",
          "title" -> "Article 2"
        )
      }

      "Note that the document with the old id has been removed" - {
        val (_, body) = GET from "/article?fields=id"
        body is arr(obj("id" -> "article_1"), obj("id" -> "article_2"))
      }

      "deleting" - {}

      "generated fields" - {}
    }
  }
}