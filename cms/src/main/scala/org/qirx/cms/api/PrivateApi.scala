package org.qirx.cms.api

import play.api.mvc.Request
import play.api.mvc.AnyContent
import play.api.mvc.Results
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

class PrivateApi(
  store: Store ~> Future,
  metadata: Metadata ~> Id,
  authentication: Authentication ~> Future)(
    implicit ec: ExecutionContext) extends Api with Results with Status
  with BuildTools with ExecutionTools {

  val executionContext = ec

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val program = programFor(request, pathAtDocumentType)
    val branched = program.foldMap(runner)
    branched.value.map(_.value.merge)
  }

  type Elements = ProgramType[(Base + Store + Metadata + Authentication + Branch[Result]#T)#T]

  def programFor(request: Request[AnyContent], pathAtDocumentType: Seq[String])(implicit e: Elements) =
    for {
      _ <- Authenticate(request) ifFalse Return(forbidden)
      (id, pathAtDocument) <- GetNextSegment(pathAtDocumentType) ifNone Return(notFound)
      meta <- GetDocumentMetadata(id) ifNone Return(notFound)
      handler = new DocumentRequestHandler(meta, request, pathAtDocument)
      result <- request.method match {
        case "POST" => handler.post
        case "GET" => handler.get
        case "PUT" => handler.put
      }
    } yield result

  class DocumentRequestHandler(meta: DocumentMetadata, request: Request[AnyContent], pathAtDocument: Seq[String])(implicit e: Elements) {

    def get =
      for {
        fieldSet <- GetFieldSetFromQueryString(request.queryString)
        (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
        _ <- Return(pathAfterId) ifNonEmpty Return(notFound)
        document <- Get(meta.id, id, fieldSet) ifNone Return(notFound)
        result <- DocumentResult(document)
      } yield result

    def list(fieldSet: Set[String]) =
      for {
        documents <- List(meta.id, fieldSet)
        result <- DocumentsResult(documents)
      } yield result

    def post =
      for {
        _ <- GetNextSegment(pathAtDocument) ifSome Return(notFound)
        json <- ToJsValue(request) ifNone Return(badRequest)
        document <- ToJsObject(json) ifNone Return(jsonExpected)
        messages <- GetMessages(meta)
        results <- Validate(meta, document, Set.empty, messages) ifEmpty
          create(document)
        result <- ValitationResultsToResult(results)
      } yield result

    def put =
      for {
        json <- ToJsValue(request) ifNone Return(badRequest)
        newDocument <- ToJsObject(json) ifNone Return(jsonExpected)
        messages <- GetMessages(meta)
        (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone Return(notFound)
        _ <- Return(pathAfterId) ifNonEmpty Return(notFound)
        oldDocument <- Get(meta.id, id, Set.empty) ifNone Return(notFound)
        fieldSet <- GetFieldSetFromQueryString(request.queryString)
        results <- Validate(meta, newDocument, fieldSet, messages) ifEmpty
          update(id, oldDocument, newDocument, fieldSet)
        result <- ValitationResultsToResult(results)
      } yield result

    def create(document: JsObject) = {
      val id = meta.idGenerator.generateFor(document)
      for {
        documentWithId <- AddId(document, id)
        _ <- Save(meta.id, id, documentWithId)
        result <- DocumentCreatedResult(id)
      } yield result
    }

    def update(id: String, oldDocument: JsObject, newDocument: JsObject, fieldSet: Set[String]) =
      for {
        merged <- Merge(oldDocument, newDocument, fieldSet)
        newId <- ExtractId(newDocument)
        actualId = newId.getOrElse(id)
        documentWithId <- AddId(merged, actualId)
        _ <- SaveIdReference(meta.id, id, newId)
        _ <- Delete(meta.id, id)
        _ <- Save(meta.id, actualId, documentWithId)
      } yield noContent
  }

  val noContent = NoContent
  val notFound = NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))
  val forbidden = Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
  val jsonExpected = UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
  val badRequest = BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))

  lazy val runner = {
    val branchRunner = BranchToFuture
    val systemRunner = SystemRunner andThen IdToBranch andThen BranchToFuture
    val metadataRunner = metadata andThen IdToBranch andThen BranchToFuture
    val authenticationRunner = authentication andThen FutureToFutureBranch
    val storeRunner = store andThen FutureToFutureBranch

    storeRunner or systemRunner or metadataRunner or authenticationRunner or branchRunner
  }
}
