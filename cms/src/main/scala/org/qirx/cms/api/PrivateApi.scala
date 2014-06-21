package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import play.api.http.Status
import play.api.libs.json.Json.obj
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Result
import play.api.mvc.Results
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsValue
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsString

class PrivateApi(documents: Seq[DocumentMetadata], authentication: RequestHeader => Future[Boolean]) extends Api with Results with Status {

  val documentMap = documents.map(d => d.id -> d).toMap

  def handleRequest(remainingPath: Seq[String], request: Request[AnyContent])(implicit ec: ExecutionContext): Future[Result] =
    authentication(request)
      .map {
        case true => handleAuthenticatedRequest(remainingPath, request)
        case false => Forbidden(obj("status" -> FORBIDDEN, "error" -> "forbidden"))
      }

  private def handleAuthenticatedRequest(remainingPath: Seq[String], request: Request[AnyContent]) = {
    val documentId = remainingPath.head
    documentMap.get(documentId).map { document =>
      handleDocumentRequest(document, request)
    }.getOrElse {
      NotFound(obj("status" -> NOT_FOUND, "error" -> "notFound"))
    }
  }

  private def handleDocumentRequest(document: DocumentMetadata, request: Request[AnyContent]) = {
    request.body.asJson.map { json =>
      handleJsonDocumentRequest(document, json)
    }.getOrElse {
      BadRequest(obj("status" -> BAD_REQUEST, "error" -> "badRequest"))
    }
  }

  private def handleJsonDocumentRequest(document: DocumentMetadata, json: JsValue) = {
    val messages = Messages.withPrefix(document.id)

    val validationResults =
      document.properties.flatMap {
        case (name, property) =>
          val value = (json \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }
      }

    if (validationResults.nonEmpty)
      UnprocessableEntity(obj("status" -> UNPROCESSABLE_ENTITY, "propertyErrors" -> validationResults))
    else
      Created(obj("id" -> "article_1"))
  }
}