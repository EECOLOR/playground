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
import qirx.Co
import org.qirx.cms.machinery.Coproduct
import org.qirx.cms.machinery.Coproduct.Transformations._
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Action
import org.qirx.cms.construction.Return
import org.qirx.cms.machinery.Program
import org.qirx.cms.construction.BranchAction
import org.qirx.cms.construction.Choose

class PrivateApi2(
  documents: Seq[DocumentMetadata],
  authentication: RequestHeader => Future[Boolean])(
    implicit system: System, ec: ExecutionContext) extends Api with Results with Status {

  case class Authenticate(request: Request[AnyContent]) extends Action[Boolean]

  trait MetadataAction[T]
  case class GetDocumentMetadata(documentId: String) extends MetadataAction[Option[DocumentMetadata]]
  case class Validate(meta: DocumentMetadata, document: JsObject, fieldSet: Set[String], messages: Messages) extends MetadataAction[Seq[JsObject]]
  case class GetMessages(meta: DocumentMetadata) extends MetadataAction[Messages]

  case class GetFieldSetFromQueryString(queryString: Map[String, Seq[String]]) extends Action[Set[String]]
  case class GetNextSegment(path: Seq[String]) extends Action[Option[(String, Seq[String])]]

  case class ToJsObject(value: JsValue) extends Action[Option[JsObject]]
  case class ToJsValue(request: Request[AnyContent]) extends Action[Option[JsValue]]

  trait ResultCreation extends Action[Result]
  case class ValitationResultsToResult(validationResults: Seq[JsObject]) extends ResultCreation
  case class DocumentsResult(documents: Seq[JsObject]) extends ResultCreation
  case class DocumentResult(document: JsObject) extends ResultCreation
  case class DocumentCreatedResult(id: String) extends ResultCreation

  trait Store[T]
  case class List(meta: DocumentMetadata, fieldSet: Set[String]) extends Store[Seq[JsObject]]
  case class Get(meta: DocumentMetadata, id: String, fieldSet: Set[String]) extends Store[Option[JsObject]]
  case class Create(meta: DocumentMetadata, document: JsObject) extends Store[String]
  case class Update(meta: DocumentMetadata, id: String, oldDocument: JsObject, newDocument: JsObject) extends Store[Unit]

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val x =
      for {
        _ <- Authenticate(request) ifFalse Return(forbidden)
        (id, pathAtDocument) <- GetNextSegment(pathAtDocumentType) ifNone Return(notFound)
        meta <- GetDocumentMetadata(id) ifNone Return(notFound)
        handler = new DocumentRequestHandler(meta, request, pathAtDocument)
        result <- request.method match {
          case "POST" => handler.post
          //case "GET" => handler.get
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
