package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.http.Status
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.~>
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Branch
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.ValueOf
import org.qirx.cms.execution.SystemToId
import org.qirx.cms.construction.api.GetNextSegment
import org.qirx.cms.construction.GetDocumentMetadata
import org.qirx.cms.construction.GetMetadata
import org.qirx.cms.construction.Authenticate
import org.qirx.cms.construction.Authentication

class MetadataApi(
    metadata: Metadata ~> Id,
    authentication: Authentication ~> Future)(
        implicit ev:ExecutionContext) extends Api {

  import BuildTools._
  import ExecutionTools._
  import Results._

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathAtDocumentType)

    program.mergeBranch executeWith runner
  }

  private type Elements = ProgramType[(System + Authentication + Metadata + Branch[Result]#T)#T]

  /**
   * The implicit parameter determines the type of the resulting program
   */
  private def programFor(request: Request[AnyContent], pathSegments: Seq[String])(implicit e: Elements) =
    for {
      _ <- Authenticate(request) ifFalse Return(forbidden)
      _ <- ValueOf(request.method == "GET") ifFalse Return(methodNotAllowed)
      (segment, rest) <- GetNextSegment(pathSegments) ifNone list
      result <- segment match {
        case "documents" => documentsRequest(rest)
        case _ => Return(notFound).asProgram
      }
    } yield result

    private def documentsRequest(pathAtDocumentId:Seq[String])(implicit e: Elements) =
      for {
        (id, pathAfterId) <- GetNextSegment(pathAtDocumentId) ifNone list
        _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
        meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      } yield ok(meta)
    
  private def list(implicit e: Elements) = 
    for {
      documentMetadata <- GetMetadata
    } yield ok(documentMetadata)

  private lazy val runner = {
      val systemRunner = SystemToId andThen IdToFuture
      val metadataRunner = metadata andThen IdToFuture
      
      authentication or systemRunner or metadataRunner
    }
}
