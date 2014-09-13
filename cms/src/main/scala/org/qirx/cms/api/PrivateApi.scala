package org.qirx.cms.api


import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.qirx.cms.construction.api._
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction._
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.Id
import org.qirx.cms.execution.SystemToId
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.construction.Index
import org.qirx.cms.construction.Authentication
import org.qirx.cms.construction.Authenticate

import play.api.mvc.Request
import play.api.mvc.AnyContent
import play.api.mvc.Result

class PrivateApi(
  store: Store ~> Future,
  index: Index ~> Future,
  metadata: Metadata ~> Id,
  authentication: Authentication ~> Future)(
    implicit ec: ExecutionContext) extends Api {

  import BuildTools._
  import ExecutionTools._
  import Results._

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathAtDocumentType)

    program.mergeBranch executeWith runner
  }

  private type Elements = ProgramType[(System + Store + Index + Metadata + Authentication + Branch[Result]#T)#T]

  /**
   * The implicit parameter determines the type of the resulting program
   */
  private def programFor(request: Request[AnyContent], pathSegments: Seq[String])(implicit e: Elements) =
    for {
      _ <- Authenticate(request) ifFalse Return(forbidden)
      method <- ValueOf(validRequestMethod(request.method)) ifNone Return(methodNotAllowed)
      (id, pathAtDocument) <- GetNextSegment(pathSegments) ifNone Return(notFound)
      meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      handler = new DocumentRequestHandler(meta, request, pathAtDocument)
      result <- method match {
        case "POST" => handler.post
        case "GET" => handler.get
        case "PUT" => handler.put
        case "PATCH" => handler.patch
        case "DELETE" => handler.delete
      }
    } yield result

  private val validRequestMethod: String => Option[String] = {
    case method @ ("GET" | "POST" | "PUT" | "PATCH" | "DELETE") => Some(method)
    case _ => None
  }

  private lazy val runner = {
    val systemRunner = SystemToId andThen IdToFuture
    val metadataRunner = metadata andThen IdToFuture

    store or index or authentication or systemRunner or metadataRunner
  }
}
