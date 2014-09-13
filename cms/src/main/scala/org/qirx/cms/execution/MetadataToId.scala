package org.qirx.cms.execution

import org.qirx.cms.construction._
import org.qirx.cms.i18n.Messages
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.ValueGenerator

class MetadataToId(documents: Seq[DocumentMetadata]) extends (Metadata ~> Id) {

  private val documentMap = documents.map(d => d.id -> d).toMap

  def transform[x] = {

    case GetMetadata => documents
    
    case GetDocumentMetadata(documentId) =>
      documentMap get documentId

    case Validate(meta, document, messages, fieldSet) =>
      meta.properties.flatMap {
        case (name, property) if fieldSet.isEmpty || (fieldSet contains name) =>

          val value = (document \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }

        case _ => None
      }.toSeq

    case RemoveConfidentialProperties(meta, document) =>
      def removeFromDocument(document: JsObject, name: String) = document - name

      confidentialPropertyNamesFrom(meta).foldLeft(document)(removeFromDocument)

    case GetMessages(meta) =>
      Messages.withPrefix(meta.id)

    case AddGeneratedProperties(meta, document) =>
      def addToDocument(document: JsObject, value: (String, ValueGenerator)) = {
        val (name, valueGenerator) = value
        document ++ obj(name -> valueGenerator.generate(name, document))
      }

      generatorsFrom(meta).foldLeft(document)(addToDocument)
  }

  /* If you can move these methods into the case
   * blocks, they have fixed a weird bug in the compiler
   */
  private def confidentialPropertyNamesFrom(meta: DocumentMetadata) =
    meta.properties.collect {
      case (name, property) if property.confidential => name
    }

  private def generatorsFrom(meta: DocumentMetadata) =
    meta.properties.collect {
      case (name, property) if property.generator.nonEmpty =>
        name -> property.generator.get
    }

}