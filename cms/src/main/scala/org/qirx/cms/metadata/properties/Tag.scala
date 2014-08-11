package org.qirx.cms.metadata.properties

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.dsl.Property

import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue

class Tag(id: String) extends Property(id) with Identifiable {

  lazy val extraJson = None
  
  protected val validTag = "[a-zA-Z0-9_-]+"

  def validate(messages: Messages, value: JsValue): Option[JsObject] =
    toType[JsString](value)
      .right.map(validateString(messages, _))
      .left.map(Option.apply)
      .merge

  def validateString(messages: Messages, value: JsString): Option[JsObject] =
    nonEmpty(messages, value)
      .right.map {
        case JsString(value) if (value matches validTag) => None
        case JsString(value) => Some(messageObj(messages, "invalidTag", value))
      }
      .left.map(Option.apply)
      .merge
}
object Tag extends Tag("tag")