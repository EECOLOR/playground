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
import org.qirx.cms.construction.Lifting._
import org.qirx.cms.construction.Free
import org.qirx.cms.construction.Action
import org.qirx.cms.construction.Branching
import org.qirx.cms.construction.Return

class PrivateApi2(
  documents: Seq[DocumentMetadata],
  authentication: RequestHeader => Future[Boolean])(
    implicit system: System, ec: ExecutionContext) extends Api with Results with Status {

  case class Authenticate(request: Request[AnyContent]) extends Action[Boolean]
  case class GetDocumentMetadata(documentId: String) extends Action[Option[DocumentMetadata]]

  case class GetFieldSetFromQueryString(queryString: Map[String, Seq[String]]) extends Action[Set[String]]
  case class GetNextSegment(path: Seq[String]) extends Action[Option[(String, Seq[String])]]

  case class ToJsObject(value: JsValue) extends Action[Option[JsObject]]
  case class ToJsValue(request: Request[AnyContent]) extends Action[Option[JsValue]]
  case class GetMessages(meta: DocumentMetadata) extends Action[Messages]

  case class ValitationResultsToResult(validationResults: Seq[JsObject]) extends Action[Result]

  case class DocumentsResult(documents: Seq[JsObject]) extends Action[Result]
  case class DocumentResult(document: JsObject) extends Action[Result]
  case class DocumentCreatedResult(id: String) extends Action[Result]

  case class Validate(meta: DocumentMetadata, document: JsObject, fieldSet: Set[String], messages: Messages) extends Action[Seq[JsObject]]
  case class List(meta: DocumentMetadata, fieldSet: Set[String]) extends Action[Seq[JsObject]]
  case class Get(meta: DocumentMetadata, id: String, fieldSet: Set[String]) extends Action[Option[JsObject]]
  case class Create(meta: DocumentMetadata, document: JsObject) extends Action[String]
  case class Update(meta: DocumentMetadata, id:String, oldDocument: JsObject, newDocument:JsObject) extends Action[Unit]

  import Branching._
  
  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val x =
      for {
        _ <- Authenticate(request) ifFalse Return(forbidden)
        (id, pathAtDocument) <- GetNextSegment(pathAtDocumentType) ifNone Return(notFound)
        meta <- GetDocumentMetadata(id) ifNone Return(notFound)
        handler = new DocumentRequestHandler(meta, request, pathAtDocument)
        result <- request.method match {
          case "GET" => handler.get
          case "POST" => handler.post
          case "PUT" => handler.put
        }
      } yield result
    ???
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
