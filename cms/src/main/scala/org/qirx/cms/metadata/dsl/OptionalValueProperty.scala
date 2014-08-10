package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages

class OptionalValueProperty(property: PropertyMetadata)
  extends WrappedProperty(property) {
  
  final val generator = None
  
  def validate(messages:Messages, value:Option[JsValue]):Option[JsObject] = 
    if (value.isEmpty) None
    else property.validate(messages, value)
}
