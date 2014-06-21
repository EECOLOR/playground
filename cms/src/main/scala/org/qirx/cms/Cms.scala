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
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import play.api.http.Status
import org.qirx.cms.api.Api
import org.qirx.cms.api.PrivateApi
import org.qirx.cms.api.MetadataApi
import org.qirx.cms.api.NoApi
import org.qirx.cms.api.PublicApi

class Cms(
  pathPrefix: String,
  authentication: RequestHeader => Future[Boolean],
  documents: Seq[DocumentMetadata]) extends Results with Status {

  def handle(request: RequestHeader, orElse: RequestHeader => Option[Handler]) =
    if (request.path startsWith pathPrefix) Some(handleRequest)
    else orElse(request)

  private val handleRequest = Action.async { request =>

    val pathParts = extractPathParts(request.path)
    val api = determineApiFor(pathParts.head)

    api.handleRequest(pathParts.tail, request)
  }

  private def extractPathParts(path: String): Seq[String] =
    path
      .replaceFirst(pathPrefix, "")
      .split("/")
      .filter(_.nonEmpty)

  private object PrivateApi extends PrivateApi(documents, authentication)

  private val determineApiFor: String => Api = {
    case "private" => PrivateApi
    case "public" => PublicApi
    case "metadata" => MetadataApi
    case _ => NoApi
  }
}