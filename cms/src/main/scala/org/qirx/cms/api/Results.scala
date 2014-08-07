package org.qirx.cms.api

import play.api.mvc.{ Results => PlayResults }
import play.api.http.Status
import play.api.libs.json.Json.obj

trait Results extends PlayResults with Status {
  private val STATUS = "status"
  private val ERROR = "error"

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
}