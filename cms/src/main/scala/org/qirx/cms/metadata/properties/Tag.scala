package org.qirx.cms.metadata.properties

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import scala.util.matching.Regex
import play.api.libs.json.Json.obj

class Tag(id: String, val pattern: String) extends Property(id) with Identifiable {

  lazy val extraJson = Some(
    obj("pattern" -> pattern)
  )

  protected def validate(messages: Messages, value: JsValue): Option[JsObject] =
    toType[JsString](value)
      .right.map(validateString(messages, _))
      .left.map(Option.apply)
      .merge

  protected def validateString(messages: Messages, value: JsString): Option[JsObject] =
    nonEmpty(messages, value)
      .right.map {
        case JsString(value) if (value matches pattern) => None
        case JsString(value) => Some(messageIdObj(messages, "invalidTag", "`" + value + "`"))
      }
      .left.map(Option.apply)
      .merge

  def determineIdentityOf(value: JsValue) = value.as[String]
}
object Tag extends Tag("tag", "[a-zA-Z0-9_-]+")