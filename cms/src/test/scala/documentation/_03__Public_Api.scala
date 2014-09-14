package documentation

import org.qirx.littlespec.Specification
import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import testUtils.PostToApplication
import testUtils.RouteRequest
import play.api.test.Helpers
import play.api.test.FakeRequest
import testUtils.PutToApplication
import testUtils.cmsName
import testUtils.PatchToApplication
import testUtils.DeleteFromApplication
import testUtils.withFixedDateTime

class _03_Public_Api extends Specification with ApiExampleSpecification {
  "# The public API" - {

    val publicApiPrefix = "/api/public"
    val privateApiPrefix = "/api/private"

    withApiExampleIntroduction(apiPrefix = publicApiPrefix) { app =>

      val POST = new PostToApplication(app, privateApiPrefix)
      val GET = new GetFromApplication(app, publicApiPrefix)
      val PATCH = new PatchToApplication(app, privateApiPrefix)
      val DELETE = new DeleteFromApplication(app, privateApiPrefix)

      """|This API provides the same GET endpoints that are available in the 
         |private API without authentication.
         |
         |The difference here is that the public API runs on the `Index` instead 
         |of the `Store`.
         |
         |The first call we make returns no documents""".stripMargin -
        example {
          val (status, body) = GET from "/article"
          status is 200
          body is arr()
        }

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
            val (status, body) = routeRequest(FakeRequest(method, "/api/public"))
            body is obj(
              "status" -> 405,
              "error" -> "methodNotAllowed"
            )
            status is 405
          }
          success
        }
      }

      "Before I can show you how the api works I create a new document" - example {
        val article = obj(
          "title" -> "Article 1",
          "tags" -> arr("tag1", "tag2")
        )

        val (status, body) = withFixedDateTime {
          POST(article) to "/article"
        }

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
            "tags" -> arr("tag1", "tag2"),
            "date" -> "2011-07-10T20:39:21+02:00"
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

      "You can retrieve a single document by specifying it's id" - example {
        val (status, body) = GET from "/article/article_1?fields=id,title"

        status is 200
        body is obj(
          "id" -> "article_1",
          "title" -> "Article 1"
        )
      }

      "If a document is updated, this change is reflected in the index as well" - example {
        val updateTitle = obj("title" -> "Article 2")
        println("patch")
        val r = PATCH("/article/article_1") using updateTitle
        println(r)

        val (status, body) = GET from "/article/article_1?fields=id,title"
        status is 200
        body is obj(
          "id" -> "article_1",
          "title" -> "Article 2"
        )
      }

      "If a documents id is updated, it's can be retrieved by it's new id." -
        example {
          val newId = obj("id" -> "article_2")
          PATCH("/article/article_1") using newId

          val (status, body) = GET from "/article/article_2?fields=id,title"
          status is 200
          body is obj(
            "id" -> "article_2",
            "title" -> "Article 2"
          )
        }

      "Note that it's also still available at it's old id" -
        example {
          val (status, body) = GET from "/article/article_1?fields=id,title"
          status is 200
          body is obj(
            "id" -> "article_2",
            "title" -> "Article 2"
          )
        }

      "The actual document with the old id has been removed" -
        example {
          val (status, body) = GET from "/article?fields=id"
          status is 200
          body is arr(obj("id" -> "article_2"))
        }

      """|Because of the public nature of the index, confidential properties 
         |are not returned.""".stripMargin - example {
        val addSecret = obj("secret" -> "A secret about Article 2")

        withFixedDateTime {
          PATCH("/article/article_2") using addSecret
        }

        val (status, body) = GET from "/article/article_2"
        status is 200
        body is obj(
          "id" -> "article_2",
          "title" -> "Article 2",
          "tags" -> arr("tag1", "tag2"),
          "date" -> "2011-07-10T20:39:21+02:00"
        )
      }

      "This is also the case when listing or searching documents." - {
        val (status, body) = GET from "/article"
        status is 200
        body is arr(
          obj(
            "id" -> "article_2",
            "title" -> "Article 2",
            "tags" -> arr("tag1", "tag2"),
            "date" -> "2011-07-10T20:39:21+02:00"
          )
        )
      }

      "When a document is deleted, it's also deleted from the index" - example {
        val (status, _) = DELETE from "/article/article_2"
        status is 204

        val (_, body) = GET from "/article"
        body is arr()
      }

      "I've added two documents to make sure I can show removal of multiple documents" -
        example {
          POST(obj("title" -> "Article 1")) to "/article"
          POST(obj("title" -> "Article 2")) to "/article"

          val (_, body) = GET from "/article?fields=id"
          body is arr(obj("id" -> "article_1"), obj("id" -> "article_2"))
        }

      "Removing multiple documents" - example {
        val (status, _) = DELETE from "/article"
        status is 204

        val (_, body) = GET from "/article"
        body is arr()
      }

      s"""|##Search
          |
          |The public API has a special endpoint called `search`. This endpoint 
          |is special in the sense that it is not handled by the `$cmsName` itself, 
          |but by the index. This allows you to implement an index that fits your 
          |needs.""".stripMargin - example {
        val (status, body) = GET from "/search/testPath"
        status is 200
        body is obj(
          "info" -> "Response from test index to search at `testPath`"
        )
      }

      s"""|##Count
          |
          |The public API has a special endpoint called `count`. This endpoint 
          |is special in the sense that it is not handled by the `$cmsName` itself, 
          |but by the index. This allows you to implement an index that fits your 
          |needs.""".stripMargin - example {
        val (status, body) = GET from "/count/testPath"
        status is 200
        body is obj(
          "info" -> "Response from test index to count at `testPath`"
        )
      }
    }
  }
}