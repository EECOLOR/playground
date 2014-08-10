package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.i18n.Messages

sealed trait Metadata[T]
case class GetDocumentMetadata(documentId: String) extends Metadata[Option[DocumentMetadata]]
case class Validate(meta: DocumentMetadata, document: JsObject, messages: Messages, fieldSet:Set[String] = Set.empty) extends Metadata[Seq[JsObject]]
case class GetMessages(meta: DocumentMetadata) extends Metadata[Messages]
case class RemoveConfidentialProperties(meta: DocumentMetadata, document: JsObject) extends Metadata[JsObject]
case class AddGeneratedProperties(meta:DocumentMetadata, document:JsObject) extends Metadata[JsObject]
