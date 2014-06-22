package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentIdGenerator
import java.text.Normalizer

object Document {
  def apply(id: String, idField: String)(properties: (String, PropertyMetadata)*): DocumentMetadata =
    new DefaultDocument(id, idField, properties.toMap)

  private class DefaultDocument(
    val id: String,
    val idField: String,
    val properties: Map[String, PropertyMetadata]) extends DocumentMetadata {

    val idGenerator =
      new DocumentIdGenerator {
        def generateFor(value: JsObject): String = {
          val stringValue = (value \ idField).as[String]
          val normalizedValue =
            Normalizer.normalize(stringValue, Normalizer.Form.NFD)
          val alphaNumericWithSpaces =
            normalizedValue.replaceAll("[^a-zA-Z0-9 _-]", "")

          val lowerCase = alphaNumericWithSpaces.toLowerCase
            
          lowerCase.replaceAll(" ", "_")
        }
        def makeUnique(id: String, value: JsObject): String = ???
      }
  }
}