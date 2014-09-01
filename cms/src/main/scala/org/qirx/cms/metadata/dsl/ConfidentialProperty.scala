package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import play.api.libs.json.Json.obj

class ConfidentialProperty[T <: PropertyMetadata](property: T) extends PropertyMetadata {
  final val id = property.id
  final val confidential = true
  final val generator = None

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    property.validate(messages, value)
    
  lazy val toJson = property.toJson ++ obj("confidential" -> true) 
}

object Confidential {
  def apply[T <: PropertyMetadata](property: T) = new ConfidentialProperty(property)
}