package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.ValueGenerator

trait GeneratableValue { _: PropertyMetadata =>
  def generated = new GeneratedValueProperty[this.type](this)
  def generate: JsValue
}

class GeneratedValueProperty[T <: PropertyMetadata with GeneratableValue](property: T)
  extends WrappedProperty(property) with PropertyValidation {

  //def once:GeneratedValueProperty = ???

  val generator = {
    val generator = new ValueGenerator {
      def generate(propertyName: String, document: JsObject): JsValue = {
        val generatedValue = property.generate

        // implemented the checking as a side effect to keep the API simple
        val validationResults = property.validate(Messages, Some(generatedValue))
        validationResults.foreach { _ =>
          sys.error(s"Generated value is invalid, value: $generatedValue\nvalidation results: $validationResults")
        }

        generatedValue
      }
    }
    Some(generator)
  }

  protected lazy val generatedErrorObj = idObj ++ obj("error" -> "generated")

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    if (value.isEmpty) None
    else Some(generatedErrorObj)

  lazy val toJson = property.toJson ++ obj("generated" -> true)
}
