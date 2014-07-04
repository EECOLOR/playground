package org.qirx.cms.execution

import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.construction.Metadata
import scala.concurrent.Future
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.GetDocumentMetadata
import org.qirx.cms.construction.Validate
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import org.qirx.cms.i18n.Messages

class MetadataHandler(documents: Seq[DocumentMetadata])
  extends (Metadata ~> Id) {

  val documentMap = documents.map(d => d.id -> d).toMap
  
  def transform[x] = {
    
    case GetDocumentMetadata(documentId) =>
      documentMap get documentId
      
    case Validate(meta, document, fieldSet, messages) =>
      meta.properties.flatMap {
        case (name, property) if fieldSet.isEmpty || (fieldSet contains name) =>
          
          val value = (document \ name).asOpt[JsValue]
          val validationResult = property.validate(messages withPrefix name, value)
          validationResult.map { _ + ("name" -> JsString(name)) }
          
        case _ => None
      }.toSeq
      
    case GetMessages(meta) =>
      Messages.withPrefix(meta.id)
  }

}