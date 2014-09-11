package org.qirx.cms.elasticsearch

import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.PropertyMetadata
import org.qirx.cms.metadata.dsl.ValueSetProperty
import org.qirx.cms.metadata.dsl.GeneratedValueProperty
import org.qirx.cms.metadata.dsl.OptionalValueProperty
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.metadata.properties.Date
import org.qirx.cms.metadata.properties.RichContent
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import play.api.libs.json.JsArray
import org.qirx.cms.metadata.dsl.ConfidentialProperty

trait PropertyMetadataIndexInformation[-T] {
  def mappings(propertyName: String): Seq[JsObject]
  def transform(propertyName: String, document: JsObject): JsObject
}

trait SimplePropertyMetadataIndexInformation[-T] extends PropertyMetadataIndexInformation[T] {
  def transform(propertyName: String, document: JsObject): JsObject = document
  def mappings(name: String) = Seq(obj(name -> mapping))
  def mapping: JsObject
}

trait LowerPriorityIndexInfo {
  implicit val identifyable = new SimplePropertyMetadataIndexInformation[Identifiable] {
    val mapping = obj(
      "type" -> "string",
      "index" -> "not_analyzed"
    )
  }
}

object PropertyMetadataIndexInformation extends LowerPriorityIndexInfo {

  implicit object date extends SimplePropertyMetadataIndexInformation[Date] {
    val mapping = obj(
      "type" -> "date",
      "format" -> "date_time_no_millis"
    )
  }

  implicit object label extends SimplePropertyMetadataIndexInformation[Label] {
    val mapping = obj(
      "type" -> "string"
    )
  }

  implicit object richContent extends PropertyMetadataIndexInformation[RichContent] {
    def mappings(name: String) = Seq(
      obj(
        name -> obj(
          "enabled" -> false
        )
      ),
      obj(
        (name + "_text") -> obj(
          "type" -> "string"
        )
      )
    )

    def transform(name: String, document:JsObject) = {
      val value = (document \ name).asOpt[JsArray]
      value
        .map(addTextValue(name, document))
        .getOrElse(document)
    }

    private def addTextValue(name: String, o: JsObject)(value: JsArray) =
      o ++ obj(name + "_text" -> RichContent.extractText(value))
  }

  class WrappingMapping[T](wrapped: PropertyMetadataIndexInformation[_]) extends PropertyMetadataIndexInformation[T] {
    def transform(name: String, document: JsObject) = wrapped.transform(name, document)
    def mappings(name: String) = wrapped.mappings(name)
  }

  implicit object confidential extends PropertyMetadataIndexInformation[ConfidentialProperty[_]] {
    def mappings(propertyName: String) = Seq.empty
    def transform(propertyName: String, document: JsObject) = document
  }

  implicit def optional[T <: PropertyMetadata](implicit m: PropertyMetadataIndexInformation[T]) =
    new WrappingMapping[OptionalValueProperty[T]](m)

  implicit def set[T <: PropertyMetadata with Identifiable](implicit m: PropertyMetadataIndexInformation[T]) =
    new WrappingMapping[ValueSetProperty[T]](m)

  implicit def generated[T <: PropertyMetadata with GeneratableValue](implicit m: PropertyMetadataIndexInformation[T]) =
    new WrappingMapping[GeneratedValueProperty[T]](m)
}