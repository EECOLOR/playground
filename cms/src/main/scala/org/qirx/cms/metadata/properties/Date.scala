package org.qirx.cms.metadata.properties

import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages

class Date(id:String) extends Property(id) with GeneratableValue {
  def validate(messages:Messages, value: JsValue): Option[JsObject] = ???
}
object Date extends Date("date")
