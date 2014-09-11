package documentation

import org.qirx.littlespec.Specification
import testUtils.PostToApplication
import testUtils.ApiExampleSpecification
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr

class _02_02_Post_Failures extends Specification with ApiExampleSpecification {

  val privateApiPrefix = "/api/private"

  withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

    val POST = new PostToApplication(app, privateApiPrefix)

    "Invalid instance" - example {
      val article = obj(
        "title" -> 0,
        "body" -> "no json",
        "tags" -> "not an array",
        "date" -> "is generated",
        "publishDate" -> "invalid date"
      )

      val (status, body) = POST(article) to "/article"

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
          ),
          obj(
            "id" -> "date",
            "name" -> "publishDate",
            "messageKey" -> "invalidDate",
            "message" -> "The value `invalid date` is not a valid date"
          )
        )
      )
    }

    "Empty instance" - example {
      val (status, body) = POST(obj()) to "/article"

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
      val (status, body) = POST("no json") to "/article"

      status is 400
      body is obj(
        "status" -> 400,
        "error" -> "badRequest"
      )
    }

    "Non exsistent endpoint" - example {
      val (status, body) = POST(obj()) to "/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Wrong document json" - example {
      val (status, body) = POST(arr()) to "/article"

      status is 422
      body is obj(
        "status" -> 422,
        "error" -> "jsonObjectExpected"
      )
    }

    "Wrong path" - example {
      val (status, body) = POST(obj()) to "/article/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }
  }
}