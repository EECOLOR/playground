package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.Json.obj
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import play.api.libs.json.Reads
import play.api.libs.json.JsObject

abstract class Property(val id: String) extends PropertyMetadata with PropertyValidation {
  val confidential = false
  
  val generator = None
  
  def ? = new OptionalValueProperty(this)

  def validate(messages: Messages, value: JsValue): Option[JsObject]
  
  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    value.map(validate(messages, _)) getOrElse Some(messageObj(messages, "required"))

}
