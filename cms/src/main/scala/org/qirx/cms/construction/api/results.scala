package org.qirx.cms.construction.api

import org.qirx.cms.construction.DirectAction
import play.api.http.Status.UNPROCESSABLE_ENTITY
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import play.api.mvc.Result
import play.api.mvc.Results.UnprocessableEntity
import play.api.mvc.Results.Ok
import play.api.mvc.Results.Created
import play.api.libs.json.Writes

trait ResultCreation extends DirectAction[Result]

case class ValitationResultsToResult(validationResults: Seq[JsObject]) extends ResultCreation {
  val result = UnprocessableEntity(obj(
    "status" -> UNPROCESSABLE_ENTITY,
    "propertyErrors" -> validationResults))
}

case class DocumentsResult(documents: Seq[JsObject]) extends ResultCreation {
  val writer = implicitly[Writes[Seq[JsObject]]]
  val result = Ok(writer writes documents)
}

case class DocumentResult(document: JsObject) extends ResultCreation {
  val result = Ok(document)
}

case class DocumentCreatedResult(id: String) extends ResultCreation {
  val result = Created(obj("id" -> id))
}
