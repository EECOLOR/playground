package org.qirx.cms.construction.api

import org.qirx.cms.construction.DirectAction
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.mvc.Request
import play.api.mvc.AnyContent

case class ToJsValue(request: Request[AnyContent]) extends DirectAction[Option[JsValue]] {
  val result = request.body.asJson
}

case class ToJsObject(value: JsValue) extends DirectAction[Option[JsObject]] {
  val result = value.asOpt[JsObject] 
}
