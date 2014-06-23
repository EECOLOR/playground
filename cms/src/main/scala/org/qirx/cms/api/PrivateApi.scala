package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.http.Status
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Result
import play.api.mvc.Results
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsValue
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsString
import org.qirx.cms.system.System
import org.qirx.cms.system.List
import play.api.libs.json.Writes
import play.api.libs.json.JsObject
import org.qirx.cms.system.Get
import org.qirx.cms.system.Update
import org.qirx.cms.system.Create

class PrivateApi(
  documents: Seq[DocumentMetadata],
  authentication: RequestHeader => Future[Boolean])(
    implicit system: System, ec: ExecutionContext) extends Api with Results with Status {

  val documentMap = documents.map(d => d.id -> d).toMap

  def handleRequest(remainingPath: Seq[String], request: Request[AnyContent]): Future[Result] =
    authentication(request)
      .flatMap {
        case true => handleAuthenticatedRequest(remainingPath, request)
        case false =>
          Future.successful {
            Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
          }
      }

  private def handleAuthenticatedRequest(remainingPath: Seq[String], request: Request[AnyContent]) = {
    val documentId = remainingPath.head
    documentMap.get(documentId).map { document =>
      handleDocumentRequest(document, request, remainingPath.tail)
    } getOrElse Future.successful(notFound)
  }

  val notFound = NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))

  private def handleDocumentRequest(document: DocumentMetadata, request: Request[AnyContent], remainingPath: Seq[String]) =
    request.method match {
      case "GET" => handleGetRequest(document, request, remainingPath)
      case "POST" => handlePostRequest(document, request)
      case "PUT" => handlePutRequest(document, request, remainingPath)
      case _ => Future.successful(badRequest)
    }

  private def handleGetRequest(document: DocumentMetadata, request: Request[AnyContent], remainingPath: Seq[String]) = {
    val fields = request.queryString.get("fields")
    val fieldSet = fields.toSet.flatten.flatMap(_.split(","))
    if (remainingPath.isEmpty) {
      system.performAction(List(document, fieldSet))
        .map(implicitly[Writes[Seq[JsObject]]].writes)
        .map(Ok(_))
    } else {
      system.performAction(Get(document, remainingPath.head, fieldSet))
        .map {
          case Some(obj) => Ok(obj)
          case None => notFound
        }
    }
  }

  private def handlePostRequest(document: DocumentMetadata, request: Request[AnyContent]) =
    request.body.asJson.map { json =>
      handleJsonDocumentCreateRequest(document, json)
    }.getOrElse {
      Future.successful(badRequest)
    }

  private def handlePutRequest(document: DocumentMetadata, request: Request[AnyContent], remainingPath: Seq[String]) = {
    val id = remainingPath.head
    if (remainingPath.tail.isEmpty)
      system.performAction(Get(document, id, Set.empty)).flatMap {
        case Some(obj) => handleDocumentUpdateRequest(document: DocumentMetadata, request: Request[AnyContent], obj)
        case None => Future.successful(notFound)
      }
    else Future.successful(notFound)
  }

  private val badRequest = BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))

  private def handleDocumentUpdateRequest(document: DocumentMetadata, request: Request[AnyContent], oldObj: JsObject) = {
    val fields = request.queryString.get("fields")
    val fieldSet = fields.toSet.flatten.flatMap(_.split(","))

    request.body.asJson.map { json =>
      handleJsonDocumentUpdateRequest(document, json, oldObj, fieldSet)
    }.getOrElse {
      Future.successful(badRequest)
    }
  }

  private def handleJsonDocumentUpdateRequest(document: DocumentMetadata, json: JsValue, oldObj: JsObject, fieldSet: Set[String]) =
    json.asOpt[JsObject]
      .map(handleJsonObjectDocumentUpdateRequest(document, _, oldObj, fieldSet))
      .getOrElse {
        Future.successful {
          UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
        }
      }

  private def handleJsonDocumentCreateRequest(document: DocumentMetadata, json: JsValue) =
    json.asOpt[JsObject]
      .map(handleJsonObjectDocumentCreateRequest(document, _))
      .getOrElse {
        Future.successful {
          UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "error" -> "jsonObjectExpected"))
        }
      }

  private def handleJsonObjectDocumentUpdateRequest(
    document: DocumentMetadata, json: JsObject, oldObj: JsObject, fieldSet: Set[String]) = {
    val messages = Messages.withPrefix(document.id)

    val validationResults =
      document.properties.flatMap {
        case (name, property) if fieldSet.isEmpty || (fieldSet contains name) =>
          val value = (json \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }
        case _ => None
      }

    if (validationResults.nonEmpty)
      Future.successful {
        UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "propertyErrors" -> validationResults))
      }
    else system.performAction(Update(document, oldObj, json, fieldSet))
      .map(_ => NoContent)
  }

  private def handleJsonObjectDocumentCreateRequest(document: DocumentMetadata, json: JsObject) = {
    val messages = Messages.withPrefix(document.id)

    val validationResults =
      document.properties.flatMap {
        case (name, property) =>
          val value = (json \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }
      }

    if (validationResults.nonEmpty)
      Future.successful {
        UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "propertyErrors" -> validationResults))
      }
    else system.performAction(Create(document, json))
      .map(id => Created(obj("id" -> id)))
  }
}