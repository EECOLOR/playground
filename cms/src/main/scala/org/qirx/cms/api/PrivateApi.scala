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

class PrivateApi(
  store: Store ~> Future,
  metadata: Metadata ~> Id,
  authentication: Authentication ~> Future)(
    implicit ec: ExecutionContext) extends Api with Results with Status with BuildTools {

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
        (id, _) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
        document <- Get(meta, id, fieldSet) ifNone Return(notFound)
        result <- DocumentResult(document)
      } yield result

    def list(fieldSet: Set[String]) =
      for {
        documents <- List(meta, fieldSet)
        result <- DocumentsResult(documents)
      } yield result

    def post =
      for {
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
        oldDocument <- Get(meta, id, Set.empty) ifNone Return(notFound)
        fieldSet <- GetFieldSetFromQueryString(request.queryString)
        results <- Validate(meta, newDocument, fieldSet, messages) ifEmpty
          update(id, oldDocument, newDocument, fieldSet)
        result <- ValitationResultsToResult(results)
      } yield result

    def create(document: JsObject) = {
      for {
        id <- Create(meta, document)
        result <- DocumentCreatedResult(id)
      } yield result
    }

    def update(id: String, oldDocument: JsObject, newDocument: JsObject, fieldSet: Set[String]) = {
      for {
        _ <- Update(meta, id, oldDocument, newDocument, fieldSet)
      } yield noContent
    }
  }

  val noContent = NoContent
  val notFound = NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))
  val forbidden = Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
  val jsonExpected = UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
  val badRequest = BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))

  val runner = {
    object ToFuture extends (Branch[Result]#Instance ~> FutureResultBranch) {
      def transform[x] = x => FutureResultBranch(Future successful x)
    }

    object ToBranch extends (Id ~> Branch[Result]#Instance) {
      def transform[x] = x => Branch[Result].Instance(Left(x))
    }

    object ToFutureBranch extends (Future ~> FutureResultBranch) {
      def transform[x] = x => FutureResultBranch(x map ToBranch.apply)
    }

    val branchRunner = ToFuture
    val systemRunner = SystemRunner andThen ToBranch andThen ToFuture
    val metadataRunner = metadata andThen ToBranch andThen ToFuture
    val authenticationRunner = authentication andThen ToFutureBranch
    val storeRunner = store andThen ToFutureBranch

    storeRunner or systemRunner or metadataRunner or authenticationRunner or branchRunner
  }
}
