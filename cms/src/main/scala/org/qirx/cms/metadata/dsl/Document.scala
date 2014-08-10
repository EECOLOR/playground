package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentIdGenerator
import java.text.Normalizer
import org.qirx.cms.metadata.Evolution
import org.qirx.cms.metadata.Evolutions
import scala.collection.immutable.ListMap

object Document {
  def apply(id: String, idField: String)(properties: (String, PropertyMetadata)*): DocumentMetadata =
    DefaultDocument(id, idField, ListMap(properties:_*))

  private case class DefaultDocument(
    id: String,
    idField: String,
    properties: ListMap[String, PropertyMetadata],
    evolutions: Evolutions = new Evolutions(Seq.empty)) extends DocumentMetadata {

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
        def makeUnique(id: String): String = id + "1"
      }

    def withEvolutions(evolutions: Evolution*) =
      copy(evolutions = this.evolutions.withEvolutions(evolutions))
  }
}