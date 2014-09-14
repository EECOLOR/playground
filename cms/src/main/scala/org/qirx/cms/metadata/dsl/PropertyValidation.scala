package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata

import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import play.api.libs.json.Reads

trait PropertyValidation { _: PropertyMetadata =>

  lazy val idObj = obj("id" -> id)

  protected def messageObj(messages: Messages, key: String, arguments: String*) =
    obj(
      "messageKey" -> key,
      "message" -> messages(key, arguments: _*)
    )

  protected def messageIdObj(messages: Messages, key: String, arguments: String*) =
    idObj ++ messageObj(messages, key, arguments: _*)

  protected lazy val invalidTypeObj = idObj ++ obj("error" -> "invalidType")

  protected def errorObj(errors: Seq[JsObject]) = idObj ++ obj("errors" -> errors)
  protected def errorObj(errors: JsObject) = idObj ++ obj("errors" -> errors)

  protected def toType[T : Reads](value: JsValue): Either[JsObject, T] =
    value.asOpt[T].toRight(invalidTypeObj)

  protected def nonEmpty(messages: Messages, value: JsString): Either[JsObject, JsString] =
    if (value.value.isEmpty) Left(messageIdObj(messages, "empty"))
    else Right(value)
}