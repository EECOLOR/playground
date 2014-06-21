package org.qirx.cms

import org.qirx.cms.metadata.DocumentMetadata
import play.api.mvc.Request
import play.api.mvc.Result
import scala.concurrent.Future
import play.api.mvc.AnyContent
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.Results
import play.api.libs.json.JsNull
import play.api.libs.concurrent.Execution.Implicits._

class Cms(
  pathPrefix: String,
  authentication: RequestHeader => Future[Boolean],
  documents: Seq[DocumentMetadata]) extends Results {

  def handle(request: RequestHeader, orElse: RequestHeader => Option[Handler]) =
    if (request.path startsWith pathPrefix) Some(handleRequest)
    else orElse(request)

  val handleRequest = Action.async { request =>
    authentication(request)
      .map {
        case true => Created(JsNull)
        case false => Forbidden(JsNull)
      }
  }
}