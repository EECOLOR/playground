package org.qirx.cms.metadata.dsl

import play.api.libs.json.Reads
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsString
import play.api.libs.json.JsArray

trait PropertyValidation { self: PropertyMetadata =>

  lazy val idObj = obj("id" -> id)

  protected def messageObj(messages: Messages, key: String, arguments:String *) =
    idObj ++
      obj(
        "messageKey" -> key,
        "message" -> messages(key, arguments: _*)
      )

  protected lazy val invalidTypeObj = idObj ++ obj("error" -> "invalidType")

  protected def errorObj(errors:Seq[JsObject]) = idObj ++ obj("errors" -> errors)
  
  protected def toType[T <: JsValue: Reads](value: JsValue): Either[JsObject, T] =
    value.asOpt[T].toRight(invalidTypeObj)
    
  protected def nonEmpty(messages:Messages, value:JsString):Either[JsObject, JsString] =
    if (value.value.isEmpty) Left(messageObj(messages, "empty"))
    else Right(value)
}