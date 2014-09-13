package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.PropertyMetadata
import org.qirx.cms.metadata.dsl.ValueSetProperty
import org.qirx.cms.metadata.dsl.GeneratedValueProperty
import org.qirx.cms.metadata.dsl.OptionalValueProperty
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Date
import org.qirx.cms.metadata.properties.RichContent
import play.api.libs.json.JsArray
import org.qirx.cms.metadata.dsl.ConfidentialProperty
import play.api.libs.json.Json.toJsFieldJsValueWrapper

trait Transformer[-T] {
  def transform(propertyName: String, document: JsObject): JsObject
}

trait LowerPriorityTrasnformer {

  implicit def noTransformation[T] = new Transformer[T] {
    def transform(propertyName: String, document: JsObject) = document
  }
}

object Transformer extends LowerPriorityTrasnformer {

  implicit object richContent extends Transformer[RichContent] {
    def transform(name: String, document: JsObject) = {
      val value = (document \ name).asOpt[JsArray]
      value
        .map(addTextValue(name, document))
        .getOrElse(document)
    }

    private def addTextValue(name: String, o: JsObject)(value: JsArray) =
      o ++ obj(name + "_text" -> RichContent.extractText(value))
  }

  class WrappingTransformer[T](wrapped: Transformer[_]) extends Transformer[T] {
    def transform(name: String, document: JsObject) = wrapped.transform(name, document)
  }

  implicit object confidential extends Transformer[ConfidentialProperty[_]] {
    def mappings(propertyName: String) = Seq.empty
    def transform(propertyName: String, document: JsObject) = document
  }

  implicit def optional[T <: PropertyMetadata](implicit m: Transformer[T]) =
    new WrappingTransformer[OptionalValueProperty[T]](m)

  implicit def set[T <: PropertyMetadata with Identifiable](implicit m: Transformer[T]) =
    new WrappingTransformer[ValueSetProperty[T]](m)

  implicit def generated[T <: PropertyMetadata with GeneratableValue](implicit m: Transformer[T]) =
    new WrappingTransformer[GeneratedValueProperty[T]](m)
}