package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj

trait GeneratableValue { self: PropertyMetadata =>
  def generated = new GeneratedValueProperty[this.type](self)
  def generate: JsValue
}

class GeneratedValueProperty[T <: PropertyMetadata with GeneratableValue](property: T)
  extends WrappedProperty(property) with PropertyValidation {

  //def once:GeneratedValueProperty = ???

  val generator = Some { () =>
    val generatedValue = property.generate

    // implemented the checking as a side effect to keep the API simple
    val validationResults = property.validate(Messages, Some(generatedValue))
    validationResults.foreach { _ =>
      sys.error(s"Generated value is invalid, value: $generatedValue\nvalidationResults: $validationResults")
    }

    generatedValue
  }

  protected lazy val generatedErrorObj = idObj ++ obj("error" -> "generated")

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    if (value.isEmpty) None
    else Some(generatedErrorObj)

  lazy val toJson = property.toJson ++ obj("generated" -> true)
}
