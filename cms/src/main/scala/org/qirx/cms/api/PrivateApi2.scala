package org.qirx.cms.api

import play.api.mvc.Request
import org.qirx.cms.metadata.DocumentMetadata
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent
import play.api.mvc.Results
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.http.Status
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import play.api.mvc.Result
import org.qirx.cms.i18n.Messages
import org.qirx.cms.machinery.BuildTools._
import org.qirx.cms.machinery.Coproduct
import org.qirx.cms.machinery.Coproduct.Transformations._
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Return
import org.qirx.cms.machinery.Program
import org.qirx.cms.machinery.Parts
import org.qirx.cms.construction.api.ToJsObject
import org.qirx.cms.construction.api.GetNextSegment
import org.qirx.cms.construction.api.ToJsValue
import org.qirx.cms.construction.api.GetFieldSetFromQueryString
import org.qirx.cms.construction.api.DocumentsResult
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.api.DocumentResult
import org.qirx.cms.construction.api.ValitationResultsToResult
import org.qirx.cms.construction.api.DocumentCreatedResult
import org.qirx.cms.construction.GetDocumentMetadata
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Authenticate
import org.qirx.cms.construction.Update
import org.qirx.cms.construction.Get
import org.qirx.cms.construction.Validate
import org.qirx.cms.construction.Create
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.List
import org.qirx.cms.construction.Authenticate
import org.qirx.cms.construction.Authentication
import org.qirx.cms.machinery.ProgramRunner
import org.qirx.cms.machinery.TypeSet
import org.qirx.cms.machinery.ProgramRunner
import org.qirx.cms.machinery.Free

class PrivateApi2(
    implicit runner: ProgramRunner[(Base + Store + Metadata + Authentication)#Out, Future]) extends Api with Results with Status {

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    new BooleanContinuation(Authenticate(request))
    
    val program =
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

    runner.run(program)
  }

  class DocumentRequestHandler(meta: DocumentMetadata, request: Request[AnyContent], pathAtDocument: Seq[String]) {

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
        results <- Validate(meta, document, Set.empty, messages) ifEmpty create(document)
        result <- ValitationResultsToResult(results)
      } yield result

    def put =
      for {
        json <- ToJsValue(request) ifNone Return(badRequest)
        newDocument <- ToJsObject(json) ifNone Return(jsonExpected)
        messages <- GetMessages(meta)
        (id, _) <- GetNextSegment(pathAtDocument) ifNone Return(notFound)
        oldDocument <- Get(meta, id, Set.empty) ifNone Return(notFound)
        fieldSet <- GetFieldSetFromQueryString(request.queryString)
        results <- Validate(meta, newDocument, fieldSet, messages) ifEmpty update(id, oldDocument, newDocument)
        result <- ValitationResultsToResult(results)
      } yield result

    def create(document: JsObject) = {
      for {
        id <- Create(meta, document)
        result <- DocumentCreatedResult(id)
      } yield result
    }

    def update(id: String, oldDocument: JsObject, newDocument: JsObject) = {
      for {
        _ <- Update(meta, id, oldDocument, newDocument)
      } yield noContent
    }
  }

  val noContent = NoContent
  val notFound = NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))
  val forbidden = Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
  val jsonExpected = UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
  val badRequest = BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))
}
