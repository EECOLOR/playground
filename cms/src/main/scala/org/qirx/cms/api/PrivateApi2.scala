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

class PrivateApi2(
  documents: Seq[DocumentMetadata],
  authentication: RequestHeader => Future[Boolean])(
    implicit system: System, ec: ExecutionContext) extends Api with Results with Status {

  import scala.language.implicitConversions
  import scala.language.higherKinds

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      this match {
        case Apply(a) => f(a)
        case Bind(i, k) =>
          Bind(i, k andThen (_ flatMap f))
      }
    def map[B](f: A => B): Free[F, B] =
      flatMap(a => Apply(f(a)))

    //for pattern matching
    def withFilter(f: Any => Boolean): Free[F, A] = this
  }

  implicit def toFree[F[_], A](fa: F[A]): Free[F, A] =
    Bind(fa, (a: A) => Apply(a))

  case class Apply[F[_], A](t: A) extends Free[F, A]
  case class Bind[F[_], A, B](i: F[A], k: A => Free[F, B]) extends Free[F, B]

  class OrAction[A, B, ResultType](
    helper: OrHelper[A, ResultType],
    value: A,
    orElse: => B) extends Action[ResultType]

  trait OrHelper[A, B] {
    def continueWithResultIf(a: A): Boolean
    def toResult(a: A): B
  }
  object OrHelper {
    implicit def optionOrHelper[A] =
      new OrHelper[Option[A], A] {
        def continueWithResultIf(a: Option[A]): Boolean = a.isDefined
        def toResult(a: Option[A]): A = a.get
      }

    implicit def simpleOrHelper[A](implicit toBoolean: A => Boolean) =
      new OrHelper[A, A] {
        def continueWithResultIf(a: A): Boolean = toBoolean(a)
        def toResult(a: A): A = a
      }

    implicit def seqOrHelper[T] = simpleOrHelper[Seq[T]](_.nonEmpty)
  }

  implicit class ActionOr[A, FreeAction, ResultType](action1: FreeAction)(
    implicit toFreeAction: FreeAction => Free[Action, A],
    helper: OrHelper[A, ResultType]) {

    def or[B](action2: Free[Action, B]): Free[Action, ResultType] =
      for {
        value1 <- action1
        value2 <- action2
        result <- new OrAction(helper, value1, value2)
      } yield result
  }

  trait Action[T]

  case class Return[T](result: T) extends Action[T]

  case class Authenticate(request: Request[AnyContent]) extends Action[Boolean]
  case class GetDocumentMetadata(documentId: String) extends Action[Option[DocumentMetadata]]

  case class GetFieldSetFromQueryString(queryString: Map[String, Seq[String]]) extends Action[Set[String]]
  case class GetNextSegment(path: Seq[String]) extends Action[Option[(String, Seq[String])]]

  case class JsValueToJsObject(value: JsValue) extends Action[Option[JsObject]]
  case class RequestToJson(request: Request[AnyContent]) extends Action[Option[JsValue]]
  case class GetMessages(meta: DocumentMetadata) extends Action[Messages]
  case class ValidateDocument(meta: DocumentMetadata, document: JsObject, fieldSet: Set[String], messages: Messages) extends Action[Seq[JsObject]]

  case class ValitationResultsToResult(validationResults: Seq[JsObject]) extends Action[Result]

  case class DocumentsToResult(documents: Seq[JsObject]) extends Action[Result]
  case class DocumentToResult(document: JsObject) extends Action[Result]

  case class ListDocuments(meta: DocumentMetadata, fieldSet: Set[String]) extends Action[Seq[JsObject]]
  case class GetDocument(meta: DocumentMetadata, id: String, fieldSet: Set[String]) extends Action[Option[JsObject]]
  case class CreateDocument(meta: DocumentMetadata, document: JsObject) extends Action[String]
  case class DocumentCreatedToResult(id: String) extends Action[Result]
  case class UpdateDocument(meta: DocumentMetadata, id:String, oldDocument: JsObject, newDocument:JsObject) extends Action[Unit]

  def handleRequest(pathAtDocumentType: Seq[String], request: Request[AnyContent]) = {

    val x =
      for {
        _ <- Authenticate(request) or Return(forbidden)
        (id, pathAtDocument) <- GetNextSegment(pathAtDocumentType) or Return(notFound)
        meta <- GetDocumentMetadata(id) or Return(notFound)
        handler = new DocumentRequestHandler(meta, request, pathAtDocumentType)
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
        (id, _) <- GetNextSegment(pathAtDocument) or list(fieldSet)
        document <- GetDocument(meta, pathAtDocument.head, fieldSet) or Return(notFound)
        result <- DocumentToResult(document)
      } yield result

    def list(fieldSet: Set[String]) =
      for {
        documents <- ListDocuments(meta, fieldSet)
        result <- DocumentsToResult(documents)
      } yield result

    def post =
      for {
        json <- RequestToJson(request) or Return(badRequest)
        document <- JsValueToJsObject(json) or Return(jsonExpected)
        messages <- GetMessages(meta)
        results <- ValidateDocument(meta, document, Set.empty, messages) or create(document)
        result <- ValitationResultsToResult(results)
      } yield result

    def put =
      for {
        json <- RequestToJson(request) or Return(badRequest)
        newDocument <- JsValueToJsObject(json) or Return(jsonExpected)
        messages <- GetMessages(meta)
        (id, _) <- GetNextSegment(pathAtDocument) or Return(notFound)
        oldDocument <- GetDocument(meta, id, Set.empty) or Return(notFound)
        fieldSet <- GetFieldSetFromQueryString(request.queryString)
        results <- ValidateDocument(meta, newDocument, fieldSet, messages) or update(id, oldDocument, newDocument)
        result <- ValitationResultsToResult(results)
      } yield result

    def create(document: JsObject) = {
      for {
        id <- CreateDocument(meta, document)
        result <- DocumentCreatedToResult(id)
      } yield result
    }

    def update(id: String, oldDocument: JsObject, newDocument: JsObject) = {
      for {
        _ <- UpdateDocument(meta, id, oldDocument, newDocument)
      } yield noContent
    }
  }

  val noContent = NoContent
  val notFound = NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))
  val forbidden = Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
  val jsonExpected = UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
  val badRequest = BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))
}