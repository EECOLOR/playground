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

trait IndexInfo[-T] {
  def mappings(propertyName: String): Seq[JsObject]
  def converter(propertyName: String): JsObject => JsObject
}

trait SimpleIndexInfo[-T] extends IndexInfo[T] {
  def converter(name: String) = identity
  def mappings(name: String) = Seq(obj(name -> mapping))
  def mapping: JsObject
}

trait LowerPriorityIndexInfo {
  implicit def identifyable = new SimpleIndexInfo[Identifiable] {
    val mapping = obj(
      "type" -> "string",
      "index" -> "not_analyzed"
    )
  }
}

object IndexInfo extends LowerPriorityIndexInfo {

  implicit object date extends SimpleIndexInfo[Date] {
    val mapping = obj(
      "type" -> "date",
      "format" -> "date_time_no_millis"
    )
  }

  implicit object label extends SimpleIndexInfo[Label] {
    val mapping = obj(
      "type" -> "string"
    )
  }

  implicit object richContent extends IndexInfo[RichContent] {
    def mappings(name: String) = Seq(
      obj(
        name -> obj(
          "enabled" -> false
        )
      ),
      obj(
        (name + ".text") -> obj(
          "type" -> "string"
        )
      )
    )

    def converter(name: String) = { o =>
      val value = (o \ name).asOpt[JsArray]
      value
        .map(addTextValue(name, o))
        .getOrElse(o)
    }

    private def addTextValue(name: String, o: JsObject)(value: JsArray) =
      o ++ obj(name + ".text" -> RichContent.extractText(value))
  }

  class WrappingMapping[T](wrapped: IndexInfo[_]) extends IndexInfo[T] {
    def converter(name: String) = wrapped.converter(name)
    def mappings(name: String) = wrapped.mappings(name)
  }

  implicit object confidential extends IndexInfo[ConfidentialProperty[_]] {
    def mappings(propertyName: String) = Seq.empty
    def converter(propertyName: String) = identity
  }

  implicit def optional[T <: PropertyMetadata](implicit m: IndexInfo[T]) =
    new WrappingMapping[OptionalValueProperty[T]](m)

  implicit def set[T <: PropertyMetadata with Identifiable](implicit m: IndexInfo[T]) =
    new WrappingMapping[ValueSetProperty[T]](m)

  implicit def generated[T <: PropertyMetadata with GeneratableValue](implicit m: IndexInfo[T]) =
    new WrappingMapping[GeneratedValueProperty[T]](m)
}