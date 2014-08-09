package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.i18n.Messages

sealed trait Metadata[T]
case class GetDocumentMetadata(documentId: String) extends Metadata[Option[DocumentMetadata]]
case class Validate(meta: DocumentMetadata, document: JsObject, fieldSet: Set[String], messages: Messages) extends Metadata[Seq[JsObject]]
case class GetMessages(meta: DocumentMetadata) extends Metadata[Messages]
case class RemoveConfidentialProperties[T](meta: DocumentMetadata, value: T)(
  implicit val converter: RemoveConfidentialProperties.Converter[T]) extends Metadata[T]
object RemoveConfidentialProperties {
  trait Converter[T] {
    def apply(value: T)(converter: JsObject => JsObject): T
  }

  implicit def convertJsObject = new Converter[JsObject] {
    def apply(value: JsObject)(converter: JsObject => JsObject) = converter(value)
  }
  implicit def convertSeqOfJsObject =
    new Converter[Seq[JsObject]] {
      def apply(value: Seq[JsObject])(converter: JsObject => JsObject) =
        value.map(converter)
    }
}
