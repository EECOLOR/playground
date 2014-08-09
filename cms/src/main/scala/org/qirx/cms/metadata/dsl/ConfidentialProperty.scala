package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages

class ConfidentialProperty(property:PropertyMetadata) extends PropertyMetadata {
  final val id = property.id
  final val confidential = true

  def validate(messages:Messages, value:Option[JsValue]):Option[JsObject] =
    property.validate(messages, value)
}

object Confidential {
  def apply(property:PropertyMetadata) = new ConfidentialProperty(property)
}