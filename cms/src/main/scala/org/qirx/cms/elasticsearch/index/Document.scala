package org.qirx.cms.elasticsearch.index

import org.qirx.cms.evolution.Evolutions
import org.qirx.cms.evolution.Evolution
import org.qirx.cms.metadata.PropertyMetadata
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.dsl.{ Document => CmsDocument }
import scala.collection.immutable.ListMap
import play.api.libs.json.Json.obj
import play.api.libs.json.JsObject
import play.api.libs.json.Json.toJsFieldJsValueWrapper

object Document {
  def apply(id: String, idField: String)(
    properties: (String, PropertyMetadata with PropertyMappings with PropertyTransformer)*): DocumentMetadata with DocumentMappings with DocumentTransformer =
    DefaultDocument(id, idField, ListMap(properties: _*))

  private case class DefaultDocument(
    id: String,
    idField: String,
    properties: ListMap[String, PropertyMetadata with PropertyMappings with PropertyTransformer],
    evolutions: Evolutions = new Evolutions(Seq.empty))
    extends DocumentMetadata
    with CmsDocument.IdFieldGenerator
    with CmsDocument.ToJson
    with DocumentMappings
    with DocumentTransformer {

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