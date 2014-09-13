package org.qirx.cms.api

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.qirx.cms.construction._
import org.qirx.cms.construction.api.GetNextSegment
import org.qirx.cms.execution.SystemToId
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>

import play.api.mvc.AnyContent
import play.api.mvc.Result
import play.api.mvc.Request

class PublicApi(
  index: Index ~> Future,
  metadata: Metadata ~> Id)(
    implicit val ec: ExecutionContext) extends Api {

  import BuildTools._
  import ExecutionTools._
  import Results._

  def handleRequest(pathSegments: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathSegments)
    program.mergeBranch executeWith runner
  }

  private type Elements = ProgramType[(System + Index + Metadata + Branch[Result]#T)#T]

  /**
   * The implicit parameter determines the type of the resulting program
   */
  private def programFor(request: Request[AnyContent], pathSegments: Seq[String])(implicit e: Elements) =
    for {
      _ <- Return(request.method == "GET") ifFalse Return(methodNotAllowed)
      (segment, rest) <- GetNextSegment(pathSegments) ifNone Return(notFound)
      result <- segment match {
        case "search" => searchRequest(request, rest)
        case "count" => countRequest(request, rest)
        case id => documentRequest(request, id, rest)
      }
    } yield result

  private def documentRequest(request: Request[AnyContent], id: String, pathAtDocument: Seq[String])(implicit e: Elements) =
    for {
      meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      handler = new IndexRequestHandler(meta, request, pathAtDocument)
      result <- handler.get
    } yield result

  private def searchRequest(request: Request[AnyContent], remainingPathSegments: Seq[String])(implicit e: Elements) =
    for {
      searchResult <- Index.Search(request, remainingPathSegments)
    } yield searchResult

  private def countRequest(request: Request[AnyContent], remainingPathSegments: Seq[String])(implicit e: Elements) =
    for {
      countResult <- Index.Count(request, remainingPathSegments)
    } yield countResult

  private lazy val runner = {
    val systemRunner = SystemToId andThen IdToFuture
    val metadataRunner = metadata andThen IdToFuture

    index or systemRunner or metadataRunner
  }
}
