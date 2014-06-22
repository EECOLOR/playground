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

class _02_Private_Api extends Specification with ApiExampleSpecification {

  "# The private API" - {

    val privateApiPrefix = "/api/private"

    withApiExampleIntroduction(apiPrefix = privateApiPrefix) { app =>

      val POST = new PostToApplication(app, privateApiPrefix)
      val GET = new GetFromApplication(app, privateApiPrefix)
      val PUT = new PutToApplication(app, privateApiPrefix)

      "The first call we make to the api returns no elements" - example {
        val (status, body) = GET from "/article"
        status is 200
        body is arr()
      }

      "So we need to create a new instance" - example {
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

      "The article can now be retrieved" - {
        val (status, body) = GET from "/article"

        status is 200
        body is arr(
          obj(
            "id" -> "article_1",
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
        )
      }

      s"""|In the scenario's where you want to list articles, you don't
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

      "We could however select a few fields to be updated" - example {
        val article = obj(
          "tags" -> arr("tag1", "tag3")
        )

        PUT(article) at "/article/article_3?fields=tags"

        val (status, body) = GET from "/article/article_3"
        status is 200
        body is obj(
          "id" -> "article_3",
          "title" -> "Article 2",
          "tags" -> arr("tag1", "tag3")
        )
      }

      "It's also possible to update the id." - example {
        val article = obj(
          "id" -> "article_2"
        )

        PUT(article) at "/article/article_3?fields=id"

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
    }
  }
}