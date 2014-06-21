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

object PublicApi extends Api with Results with Status {
  def handleRequest(remainingPath: Seq[String], request: Request[AnyContent])(implicit ec:ExecutionContext): Future[Result] =
    Future.successful {
      Ok(arr(obj("id" -> "article_1", "label" -> "Article 1")))
    }
}