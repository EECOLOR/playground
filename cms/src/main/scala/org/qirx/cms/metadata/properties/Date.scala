package org.qirx.cms.metadata.properties

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import org.qirx.cms.system.IsoDate
import org.joda.time.DateTime

class Date(id: String) extends Property(id) with GeneratableValue {

  lazy val extraJson = None
  
  def generate: JsValue = IsoDate.writes writes DateTime.now

  protected def validate(messages: Messages, value: JsValue): Option[JsObject] =
    toType[JsString](value)
      .right.map(validateDateString(messages, _))
      .left.map(Option.apply)
      .merge

  def validateDateString(messages: Messages, value: JsString): Option[JsObject] =
    value.validate(IsoDate.reads).fold(
      invalid = _ => Some(messageIdObj(messages, "invalidDate", "`" + value.value + "`")),
      valid = _ => None
    )

}
object Date extends Date("date")