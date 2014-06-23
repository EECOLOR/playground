package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import play.api.http.Status
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Results

object MetadataApi extends Api with Results with Status {
  def handleRequest(remainingPath: Seq[String], request: Request[AnyContent]): Future[Result] =
    Future.successful {
      Ok(
        arr(
          obj(
            "id" -> "article",
            "properties" -> arr(
              obj(
                "id" -> "label",
                "name" -> "title"
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
      )
    }
}