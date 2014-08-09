package org.qirx.cms.api

import play.api.mvc.Request
import play.api.mvc.AnyContent
import play.api.http.Status
import play.api.mvc.Result
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import org.qirx.cms.construction.api._
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction._
import org.qirx.cms.machinery.FutureResultBranch
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.Id
import org.qirx.cms.execution.SystemRunner
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.construction.Index

class PrivateApi(
  store: Store ~> Future,
  index: Index ~> Future,
  metadata: Metadata ~> Id,
  authentication: Authentication ~> Future)(
    implicit val ec: ExecutionContext) extends Api with Results
  with BuildTools with ExecutionTools {

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathAtDocumentType)
    val branched = program.foldMap(runner)
    branched.value.map(_.value.merge)
  }

  private type Elements = ProgramType[(Base + Store + Index + Metadata + Authentication + Branch[Result]#T)#T]

  /**
   * The implicit parameter determines the type of the resulting program
   */
  private def programFor(request: Request[AnyContent], pathSegments: Seq[String])(implicit e: Elements) =
    for {
      _ <- Authenticate(request) ifFalse Return(forbidden)
      method <- Return(validRequestMethod(request.method)) ifNone Return(methodNotAllowed)
      (id, pathAtDocument) <- GetNextSegment(pathSegments) ifNone Return(notFound)
      meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      handler = new DocumentRequestHandler(meta, request, pathAtDocument)
      result <- method match {
        case "POST" => handler.post
        case "GET" => handler.get
        case "PUT" => handler.put
      }
    } yield result

  private val validRequestMethod: String => Option[String] = {
    case method @ ("GET" | "POST" | "PUT") => Some(method)
    case _ => None
  }

  private lazy val runner = {
    val branchRunner = BranchToFuture
    val systemRunner = SystemRunner andThen IdToBranch andThen BranchToFuture
    val metadataRunner = metadata andThen IdToBranch andThen BranchToFuture
    val authenticationRunner = authentication andThen FutureToFutureBranch
    val storeRunner = store andThen FutureToFutureBranch
    val indexRunner = index andThen FutureToFutureBranch

    storeRunner or indexRunner or systemRunner or metadataRunner or authenticationRunner or branchRunner
  }
}
