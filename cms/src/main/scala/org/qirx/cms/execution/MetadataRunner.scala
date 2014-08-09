package org.qirx.cms.execution

import org.qirx.cms.construction.GetDocumentMetadata
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Validate
import org.qirx.cms.i18n.Messages
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.construction.RemoveConfidentialProperties

class MetadataRunner(documents: Seq[DocumentMetadata]) extends (Metadata ~> Id) {

  val documentMap = documents.map(d => d.id -> d).toMap

  def transform[x] = {

    case GetDocumentMetadata(documentId) =>
      documentMap get documentId

    case Validate(meta, document, messages) =>
      meta.properties.flatMap {
        case (name, property) =>
          val value = (document \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }
      }.toSeq

    case RemoveConfidentialProperties(meta, document) =>
      def removeFromDocument(document: JsObject, name: String) = document - name

      confidentialPropertyNamesFrom(meta).foldLeft(document)(removeFromDocument)

    case GetMessages(meta) =>
      Messages.withPrefix(meta.id)

  }

  private def confidentialPropertyNamesFrom(meta: DocumentMetadata) =
    meta.properties.collect {
      case (name, property) if property.confidential => name
    }

}