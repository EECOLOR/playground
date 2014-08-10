package documentation

import org.qirx.littlespec.Specification
import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj

class _04_MetadataApi extends Specification with ApiExampleSpecification {

  "# The metadata API" - {

    val metadaApiPrefix = "/api/metadata"

    withApiExampleIntroduction(apiPrefix = metadaApiPrefix) { app =>

      val GET = new GetFromApplication(app, metadaApiPrefix)

      """|The metadata API is a read only API, metadata can only be changed at
         |compile time. This is by design, the compiler does a lot of complicated
         |validation that would otherwise be tricky to implement. On top of that,
         |it's nice to see the metadata evolve alongside with your code. Modern 
         |version control is a great help in tracking and managing changes. 
         |
         |You can get the metadata for all documents like this:""".stripMargin - {
        val (status, body) = GET from "/documents"

        status is 200
        body is arr(
          obj(
            "id" -> "article",
            "properties" -> arr(
              obj(
                "id" -> "label",
                "name" -> "title"
              ),
              obj(
                "id" -> "label",
                "name" -> "secret"
              ),
              obj(
                "id" -> "rich_content",
                "name" -> "body",
                "optional" -> true,
                "extra" -> obj(
                  "allowedElements" -> arr(
                    "strong", "em", "ul", "ol", "li", "span[class|lang]",
                    "a[href|hreflang|title|target]", "br", "p[class|lang]")
                )
              ),
              obj(
                "id" -> "tag",
                "name" -> "tags",
                "set" -> true,
                "nonEmpty" -> false
              ),
              obj(
                "id" -> "date",
                "name" -> "date",
                "generated" -> true
              )
            )
          )
        )
      }
    }
  }
}