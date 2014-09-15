package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj

abstract class Property(val id: String) extends PropertyMetadata with PropertyValidation {
  val confidential = false

  val generator = None

  def ? = new OptionalValueProperty[this.type](this)

  protected def validate(messages: Messages, value: JsValue): Option[JsObject]

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    value.map(validate(messages, _)) getOrElse Some(messageIdObj(messages, "required"))

  lazy val toJson = {
    val idObj = obj("id" -> id)
    extraJson
      .map(extra => obj("extra" -> extra))
      .fold(ifEmpty = idObj)(idObj ++ _)
  }

  protected def extraJson: Option[JsObject]
}
