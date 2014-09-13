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

trait Mappings[-T] {
  def mappings(propertyName: String): Seq[JsObject]
}

trait Mapping[-T] extends Mappings[T] {
  def mappings(name: String) = Seq(obj(name -> mapping))
  def mapping: JsObject
}

trait LowerPriorityMapping {
  implicit val identifyable = new Mapping[Identifiable] {
    val mapping = obj(
      "type" -> "string",
      "index" -> "not_analyzed"
    )
  }
}

object Mappings extends LowerPriorityMapping {

  implicit object date extends Mapping[Date] {
    val mapping = obj(
      "type" -> "date",
      "format" -> "date_time_no_millis"
    )
  }

  implicit object label extends Mapping[Label] {
    val mapping = obj(
      "type" -> "string"
    )
  }

  implicit object richContent extends Mappings[RichContent] {
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
  }

  class WrappingMapping[T](wrapped: Mappings[_]) extends Mappings[T] {
    def mappings(name: String) = wrapped.mappings(name)
  }

  implicit object confidential extends Mappings[ConfidentialProperty[_]] {
    def mappings(propertyName: String) = Seq.empty
  }

  implicit def optional[T <: PropertyMetadata](implicit m: Mappings[T]) =
    new WrappingMapping[OptionalValueProperty[T]](m)

  implicit def set[T <: PropertyMetadata with Identifiable](implicit m: Mappings[T]) =
    new WrappingMapping[ValueSetProperty[T]](m)

  implicit def generated[T <: PropertyMetadata with GeneratableValue](implicit m: Mappings[T]) =
    new WrappingMapping[GeneratedValueProperty[T]](m)
}