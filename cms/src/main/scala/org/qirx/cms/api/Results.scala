package org.qirx.cms.api

import play.api.http.Status
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes
import play.api.mvc.{Results => PlayResults}

trait Results extends PlayResults with Status {
  val STATUS = "status"
  val ERROR = "error"
  val PROPERTY_ERRORS = "propertyErrors"
  val ID = "id"

  val noContent = NoContent

  val notFound = NotFound(obj(
    STATUS -> NOT_FOUND, ERROR -> "notFound"))

  val forbidden = Forbidden(obj(
    STATUS -> FORBIDDEN, ERROR -> "forbidden"))

  val jsonExpected = UnprocessableEntity(obj(
    STATUS -> UNPROCESSABLE_ENTITY, ERROR -> "jsonObjectExpected"))

  val badRequest = BadRequest(obj(
    STATUS -> BAD_REQUEST, ERROR -> "badRequest"))

  val methodNotAllowed = MethodNotAllowed(obj(
    STATUS -> METHOD_NOT_ALLOWED, ERROR -> "methodNotAllowed"))

  def valitationResultsToResult(validationResults: Seq[JsObject]) =
    UnprocessableEntity(obj(
      STATUS -> UNPROCESSABLE_ENTITY,
      PROPERTY_ERRORS -> validationResults))

  def ok[T: Writes](value: T) = {
    val writer = implicitly[Writes[T]]
    Ok(writer writes value)
  }

  def created(id: String) =
    Created(idObj(id))

  def idObj(id: String) = obj(ID -> id)
}

object Results extends Results