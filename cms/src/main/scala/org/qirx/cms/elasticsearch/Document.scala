package org.qirx.cms.elasticsearch

import org.qirx.cms.evolution.Evolutions
import org.qirx.cms.evolution.Evolution
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.PropertyMetadata
import scala.collection.immutable.ListMap
import org.qirx.cms.metadata.dsl.{ Document => CmsDocument }
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsObject

object Document {
  def apply(id: String, idField: String)(
    properties: (String, PropertyMetadata with PropertyIndexInformation)*): DocumentMetadata with DocumentMapping =
    DefaultDocument(id, idField, ListMap(properties: _*))

  private case class DefaultDocument(
    id: String,
    idField: String,
    properties: ListMap[String, PropertyMetadata with PropertyIndexInformation],
    evolutions: Evolutions = new Evolutions(Seq.empty))
    extends DocumentMetadata
    with CmsDocument.IdFieldGenerator
    with CmsDocument.ToJson
    with DocumentMapping {

    def withEvolutions(evolutions: Evolution*) =
      copy(evolutions = this.evolutions.withEvolutions(evolutions))

    private lazy val propertyMappings =
      properties
        .flatMap { case (name, property) => property.mappings(name) }
        .foldLeft(obj())(_ ++ _)

    private lazy val idMapping = obj(
      "id" -> obj(
        "type" -> "string",
        "index" -> "not_analyzed"
      )
    )

    lazy val mapping = obj(
      id -> obj(
        "dynamic" -> "strict",
        "date_detection" -> false,
        "properties" -> (propertyMappings ++ idMapping)
      )
    )

    def transform(document: JsObject): JsObject =
      properties.foldLeft(document) {
        case (document, (name, property)) => property.transform(name, document)
      }
  }
}