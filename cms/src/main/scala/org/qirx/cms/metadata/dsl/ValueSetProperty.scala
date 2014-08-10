package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages

import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue

trait Identifiable { self: Property =>
  private def set(nonEmpty: Boolean) = new ValueSetProperty(self, nonEmpty)
  //def + = set(nonEmpty = true)
  def * = set(nonEmpty = false)
}

class ValueSetProperty(property: Property with Identifiable, nonEmpty: Boolean)
  extends WrappedProperty(property) with PropertyValidation {

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
    val errors = values.flatMap(property.validate(messages, _))
    if (errors.isEmpty) None
    else Some(errorObj(errors))
  }
}
