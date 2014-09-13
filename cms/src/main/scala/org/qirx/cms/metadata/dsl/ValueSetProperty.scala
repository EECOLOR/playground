package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.PropertyMetadata

trait Identifiable { _: PropertyMetadata =>
  private def set(nonEmpty: Boolean) = new ValueSetProperty[this.type](this, nonEmpty)
  //def + = set(nonEmpty = true)
  def * = set(nonEmpty = false)
}

class ValueSetProperty[T <: PropertyMetadata with Identifiable](property: T, nonEmpty: Boolean)
  extends WrappedProperty[T](property) with PropertyValidation {

  final val generator = None
  
  protected val allowEmpty = !nonEmpty

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    value match {
      case None =>
        if (allowEmpty) None
        else Some(messageObj(messages, "empty"))
      case Some(value) =>
        toType[JsArray](value)
          .right.map(validateArray(messages, _))
          .left.map(Option.apply)
          .merge
    }
  
  protected def validateArray(messages:Messages, array:JsArray):Option[JsObject] = {
    val values = array.as[Seq[JsValue]]
    val errors = values.flatMap(value => property.validate(messages, Some(value)))
    if (errors.isEmpty) None
    else Some(errorObj(errors))
  }
  
  lazy val toJson = property.toJson ++ obj("set" -> true, "nonEmpty" -> nonEmpty)
}
