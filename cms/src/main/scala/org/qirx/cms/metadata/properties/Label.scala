package org.qirx.cms.metadata.properties

import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages

class Label(id: String) extends Property(id) {
  
  def validate(messages:Messages, value: JsValue): Option[JsObject] =
    toType[JsString](value)
      .right.map(validateString(messages, _))
      .left.map(Option.apply)
      .merge
      
   def validateString(messages:Messages, value:JsString):Option[JsObject] =
     nonEmpty(messages, value).left.toOption
}
object Label extends Label("label")