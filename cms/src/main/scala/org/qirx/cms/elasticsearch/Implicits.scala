package org.qirx.cms.elasticsearch

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.PropertyMetadata

object Implicits {
  import scala.language.implicitConversions

  implicit def propertyWithMetadataIndexInformation[T <: PropertyMetadata](property: T)(
    implicit propertyMetadataIndexInformation: PropertyMetadataIndexInformation[T]): PropertyMetadata with PropertyIndexInformation =
    new PropertyMetadata with PropertyIndexInformation {
      val id = property.id
      val confidential = property.confidential
      val generator = property.generator

      def validate(messages: Messages, value: Option[JsValue]) =
        property.validate(messages, value)

      lazy val toJson = property.toJson

      def mappings(name: String) = propertyMetadataIndexInformation.mappings(name)
      def transform(name: String, document: JsObject) = propertyMetadataIndexInformation.transform(name, document)
    }
}
