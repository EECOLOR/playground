package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.i18n.Messages

sealed trait Metadata[T]
case class GetDocumentMetadata(documentId: String) extends Metadata[Option[DocumentMetadata]]
case class Validate(meta: DocumentMetadata, document: JsObject, fieldSet: Set[String], messages: Messages) extends Metadata[Seq[JsObject]]
case class GetMessages(meta: DocumentMetadata) extends Metadata[Messages]
