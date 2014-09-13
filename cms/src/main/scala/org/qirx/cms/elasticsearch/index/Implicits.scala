package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import scala.language.implicitConversions
import org.qirx.cms.metadata.PropertyMetadata

object Implicits {
  import scala.language.implicitConversions

  implicit def addMapppingsAndTransformer[T <: PropertyMetadata](property: T)(
    implicit m: Mappings[T], t:Transformer[T]): PropertyMetadata with PropertyMappings with PropertyTransformer =
    new PropertyMetadata with PropertyMappings with PropertyTransformer {
      val id = property.id
      val confidential = property.confidential
      val generator = property.generator

      def validate(messages: Messages, value: Option[JsValue]) =
        property.validate(messages, value)

      lazy val toJson = property.toJson

      def mappings(name: String) = m.mappings(name)
      def transform(name: String, document: JsObject) = t.transform(name, document)
    }
}
