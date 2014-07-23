package org.qirx.cms

import scala.concurrent.Future
import scala.language.higherKinds
import scala.language.implicitConversions

import org.qirx.cms.api.Api
import org.qirx.cms.api.MetadataApi
import org.qirx.cms.api.NoApi
import org.qirx.cms.api.PrivateApi
import org.qirx.cms.api.PublicApi
import org.qirx.cms.construction.Store
import org.qirx.cms.execution.AuthenticationRunner
import org.qirx.cms.execution.MetadataRunner
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata

import play.api.http.Status
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Action
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Results

class Cms(
  pathPrefix: String,
  authenticate: RequestHeader => Future[Boolean],
  documents: Seq[DocumentMetadata])(implicit store: Store ~> Future) extends Results with Status {

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

  val metadata = new MetadataRunner(documents)
  val authentication = new AuthenticationRunner(authenticate)

  lazy val privateApi = new PrivateApi(store, metadata, authentication)
  
  private val determineApiFor: String => Api = {
    case "private" => privateApi
    case "public" => PublicApi
    case "metadata" => MetadataApi
    case _ => NoApi
  }
}