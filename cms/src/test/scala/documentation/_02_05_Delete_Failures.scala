package documentation

import testUtils.ApiExampleSpecification
import testUtils.PatchToApplication
import org.qirx.littlespec.Specification
import testUtils.DeleteFromApplication
import play.api.libs.json.Json.obj

class _02_05_Delete_Failures extends Specification with ApiExampleSpecification {

  val privateApiPrefix = "/api/private"

  withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

    val DELETE = new DeleteFromApplication(app, privateApiPrefix)
    
    "Invalid id" - example {
      val (status, body) = DELETE from "/article/not_existent"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Non exsistent endpoint" - example {
      val (status, body) = DELETE from "/non_existing/article_1"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Wrong path" - example {
      val (status, body) = DELETE from "/article/article_1/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }
  }
}