package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.http.Status
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import org.qirx.cms.machinery.~>
import org.qirx.cms.machinery.Id
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.api.GetNextSegment
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.construction.GetDocumentMetadata
import org.qirx.cms.execution.SystemRunner
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.construction.Branch
import org.qirx.cms.construction.Return
import org.qirx.cms.machinery.Free
import org.qirx.cms.construction.Index
import org.qirx.cms.construction.Index._

class PublicApi(
  index: Index ~> Future,
  store: Store ~> Future,
  metadata: Metadata ~> Id)(
    implicit val ec: ExecutionContext) extends Api with Results
  with BuildTools with ExecutionTools {

  def handleRequest(pathSegments: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathSegments)
    val branched = program.foldMap(runner)
    branched.value.map(_.value.merge)
  }

  private type Elements = ProgramType[(Base + Index + Store + Metadata + Branch[Result]#T)#T]

  /**
   * The implicit parameter determines the type of the resulting program
   */
  private def programFor(request: Request[AnyContent], pathSegments: Seq[String])(implicit e: Elements) =
    for {
      _ <- Return(validRequestMethod(request.method)) ifNone Return(methodNotAllowed)
      (segment, rest) <- GetNextSegment(pathSegments) ifNone Return(notFound)
      result <- segment match {
        case "search" => searchRequest(request, rest)
        case id => documentRequest(request, id, rest)
      }
    } yield result

  private val validRequestMethod: String => Option[String] = {
    case get @ "GET" => Some(get)
    case _ => None
  }

  private def documentRequest(request: Request[AnyContent], id: String, pathAtDocument: Seq[String])(implicit e: Elements) =
    for {
      meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      handler = new IndexRequestHandler(meta, request, pathAtDocument)
      result <- handler.get
    } yield result

  private def searchRequest(request: Request[AnyContent], remainingPathSegments: Seq[String])(implicit e: Elements) =
    for {
      searchResult <- Search(request, remainingPathSegments)
    } yield searchResult

  private lazy val runner = {
    val branchRunner = BranchToFuture
    val systemRunner = SystemRunner andThen IdToBranch andThen BranchToFuture
    val metadataRunner = metadata andThen IdToBranch andThen BranchToFuture
    val indexRunner = index andThen FutureToFutureBranch
    val storeRunner = store andThen FutureToFutureBranch

    indexRunner or storeRunner or systemRunner or metadataRunner or branchRunner
  }
}
