package org.qirx.cms.metadata.dsl

import scala.collection.immutable.ListMap

import org.qirx.cms.metadata.DefaultDocumentIdGenerator
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.evolution.Evolution
import org.qirx.cms.evolution.Evolutions
import org.qirx.cms.metadata.PropertyMetadata

object Document {
  def apply(id: String, idField: String)(properties: (String, PropertyMetadata)*): DocumentMetadata =
    DefaultDocument(id, idField, ListMap(properties: _*))

  private case class DefaultDocument(
    id: String,
    idField: String,
    properties: ListMap[String, PropertyMetadata],
    evolutions: Evolutions = new Evolutions(Seq.empty)) extends DocumentMetadata {

    val idGenerator = new DefaultDocumentIdGenerator(idField)

    def withEvolutions(evolutions: Evolution*) =
      copy(evolutions = this.evolutions.withEvolutions(evolutions))
  }
}