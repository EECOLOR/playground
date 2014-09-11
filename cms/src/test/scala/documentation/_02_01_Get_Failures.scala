package documentation

import org.qirx.littlespec.Specification
import testUtils.PostToApplication
import testUtils.ApiExampleSpecification
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import testUtils.GetFromApplication
import org.qirx.cms.Cms
import testUtils.TestEnvironment
import scala.concurrent.Future
import testUtils.TestApplication
import testUtils.codeString
import play.api.mvc.RequestHeader

class _02_01_Get_Failures extends Specification with ApiExampleSpecification {

  val privateApiPrefix = "/api/private"

  withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

    val GET = new GetFromApplication(app, privateApiPrefix)
    val POST = new PostToApplication(app, privateApiPrefix)

    "For these examples we make sure an article exists" - example {
      val (status, body) = POST(obj("title" -> "Article 1")) at "/article"
      status is 201
      body is obj(
        "id" -> "article_1"
      )
    }

    "Non exsistent endpoint" - example {
      val (status, body) = GET from "/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Non existent doucment" - {
      val (status, body) = GET from "/article/non_existing"
      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Wrong path" - example {
      val (status, body) = GET from "/article/article_1/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }
  }

  val failingAuthentication = codeString {
    request:RequestHeader => Future.successful(false)
  }
  
  s"""|Authentication failure (using the following method authenticate method)
      |```scala
      |  $failingAuthentication
      |```""".stripMargin - {
    val cms = new Cms(
      pathPrefix = "/api",
      authenticate = failingAuthentication.value,
      environment = new TestEnvironment,
      documents = Seq.empty
    )

    val GET = new GetFromApplication(TestApplication(cms), privateApiPrefix)

    example {
      val (status, body) = GET from "/anything"

      status is 403
      body is obj(
        "status" -> 403,
        "error" -> "forbidden"
      )
    }
  }
}