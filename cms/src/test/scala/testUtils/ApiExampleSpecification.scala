package testUtils

import org.qirx.littlespec.Specification
import org.qirx.cms.Cms
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.metadata.properties.Date
import scala.concurrent.Future
import org.qirx.cms.metadata.dsl.Document
import scala.io.Source
import play.api.test.FakeApplication

trait ApiExampleSpecification extends Example { self: Specification =>

  def withApiExampleIntroduction(apiPrefix: String)(specifications: FakeApplication => FragmentBody) = {

    val pathPrefix = "/api"

    implicit val system = new TestSystem

    val cms = codeString {
      new Cms(
        pathPrefix = "/api",
        authentication = { _ => Future.successful(true) },
        documents = Seq(
          Document(id = "article", idField = "title")(
            "title" -> Label,
            "body" -> RichContent.?,
            "tags" -> Tag.*,
            "date" -> Date.generated
          )
        )
      )
    }

    val app = TestApplication(cms.value)

    val messages = {
      val messagesUrl = app.classloader.getResource("conf/messages")
      Source.fromURL(messagesUrl).mkString
    }

    s"""|For the examples in this specification we use the following
        |`$cmsName` definition:
        |
        |```scala
        |$cms
        |```
        |
        |For all requests we are using `$apiPrefix` as a prefix.
        |
        |This is the `messages` file we are using for human friendly messages:
        |```
        |$messages
        |```""".stripMargin - {

      specifications(app)
    }
  }
}