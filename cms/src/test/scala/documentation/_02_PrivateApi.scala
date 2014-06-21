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

object _02_PrivateApi extends Specification with Example {

  "# The private API" - {

    val pathPrefix = "/api"
    val privateApiPrefix = pathPrefix + "/private"

    val cms = codeString {
      new Cms(
        pathPrefix = "/api",
        authentication = { _ => Future.successful(true) },
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

    val app = TestApplication(cms.value)
      .copy(additionalConfiguration = Map("messages.path" -> "conf"))

    val POST = new PostToApplication(app, privateApiPrefix)

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
        |For all requests we are using `$privateApiPrefix` as a prefix.
        |
        |This is the `messages` file we are using for human friendly messages:
        |```
        |$messages
        |```""".stripMargin - {

      "Creating a new instance" - example {
        val article = obj(
          "title" -> "Article 1",
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
      
      "Invalid instance" - example {
        val article = obj(
          "title" -> 0,
          "body" -> "no json",
          "tags" -> "not an array",
          "date" -> "is generated"
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
    }
  }
}