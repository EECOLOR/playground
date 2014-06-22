package documentation

import org.qirx.littlespec.Specification
import testUtils.PostToApplication
import testUtils.ApiExampleSpecification
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import testUtils.PutToApplication

object _02_03_Put_Failures extends Specification with ApiExampleSpecification {

  val privateApiPrefix = "/api/private"

  withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

    val PUT = new PutToApplication(app, privateApiPrefix)
    val POST = new PostToApplication(app, privateApiPrefix)

    "For these examples we make sure an article exists" - example {
      val (status, body) = POST(obj("title" -> "Article 1")) at "/article"
      status is 201
      body is obj(
        "id" -> "article_1"
      )
    }

    "Invalid id" - example {
      val (status, body) = PUT(obj()) at "/article/not_existent"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Invalid instance" - example {
      val article = obj(
        "title" -> 0,
        "body" -> "no json",
        "tags" -> "not an array",
        "date" -> "is generated"
      )

      val (status, body) = PUT(article) at "/article/article_1"

      status is 422
      body is obj(
        "status" -> 422,
        "propertyErrors" -> arr(
          obj(
            "id" -> "label",
            "name" -> "title",
            "error" -> "invalidType"
          ),
          obj(
            "id" -> "rich_content",
            "name" -> "body",
            "error" -> "invalidType"
          ),
          obj(
            "id" -> "tag",
            "name" -> "tags",
            "error" -> "invalidType"
          ),
          obj(
            "id" -> "date",
            "name" -> "date",
            "error" -> "generated"
          )
        )
      )
    }

    "Empty instance" - example {
      val (status, body) = PUT(obj()) at "/article/article_1"

      status is 422
      body is obj(
        "status" -> 422,
        "propertyErrors" -> arr(
          obj(
            "id" -> "label",
            "name" -> "title",
            "messageKey" -> "required",
            "message" -> "The field `Title` can not be empty"
          )
        )
      )
    }

    "Malformed json" - example {
      val (status, body) = PUT("no json") at "/article/article_1"

      status is 400
      body is obj(
        "status" -> 400,
        "error" -> "badRequest"
      )
    }

    "Non exsistent endpoint" - example {
      val (status, body) = PUT(obj()) at "/non_existing/article_1"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Wrong document json" - example {
      val (status, body) = PUT(arr()) at "/article/article_1"

      status is 422
      body is obj(
        "status" -> 422,
        "error" -> "jsonObjectExpected"
      )
    }
  }
}