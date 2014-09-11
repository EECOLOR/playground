package documentation

import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication
import testUtils.PostToApplication
import org.qirx.littlespec.Specification
import play.api.libs.json.Json.obj
import org.qirx.cms.Cms
import testUtils.TestEnvironment
import testUtils.codeString
import play.api.mvc.RequestHeader
import testUtils.TestApplication
import scala.concurrent.Future

class _04_01_Get_Failures extends Specification with ApiExampleSpecification {

  val metadataApiPrefix = "/api/metadata"

  withApiExampleIntroduction(apiPrefix = metadataApiPrefix) { app =>

    val GET = new GetFromApplication(app, metadataApiPrefix)

    "Non exsistent endpoint" - example {
      val (status, body) = GET from "/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Non existent doucment type" - {
      val (status, body) = GET from "/documents/non_existing"
      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }

    "Wrong path" - example {
      val (status, body) = GET from "/documents/article/non_existing"

      status is 404
      body is obj(
        "status" -> 404,
        "error" -> "notFound"
      )
    }
  }

  val failingAuthentication = codeString {
    request: RequestHeader => Future.successful(false)
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

    val GET = new GetFromApplication(TestApplication(cms), metadataApiPrefix)

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