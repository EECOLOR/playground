package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages

trait GeneratableValue { self: PropertyMetadata =>
  def generated = new GeneratedValueProperty(self)
}

class GeneratedValueProperty(property: PropertyMetadata with GeneratableValue)
  extends WrappedProperty(property) with PropertyValidation {
 
  protected lazy val generatedErrorObj = idObj ++ obj("error" -> "generated")
  
  def validate(messages:Messages, value:Option[JsValue]):Option[JsObject] =
    if (value.isEmpty) None
    else Some(generatedErrorObj)
    
}
