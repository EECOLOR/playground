package org.qirx.cms.metadata.dsl

import scala.collection.immutable.ListMap
import org.qirx.cms.metadata.DefaultDocumentIdGenerator
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.evolution.Evolution
import org.qirx.cms.evolution.Evolutions
import org.qirx.cms.metadata.PropertyMetadata
import play.api.libs.json.Json.obj

object Document {
  def apply(id: String, idField: String)(properties: (String, PropertyMetadata)*): DocumentMetadata =
    DefaultDocument(id, idField, ListMap(properties: _*))

  trait IdFieldGenerator { self: DocumentMetadata =>
    val idField:String
    val idGenerator = new DefaultDocumentIdGenerator(idField)
  }
  
  trait ToJson { self: DocumentMetadata =>
    lazy val toJson =
      obj(
        "id" -> id,
        "properties" -> properties.map {
          case (name, property) => property.toJson ++ obj("name" -> name)
        }
      )
  }
    
  private case class DefaultDocument(
    id: String,
    idField: String,
    properties: ListMap[String, PropertyMetadata],
    evolutions: Evolutions = new Evolutions(Seq.empty)) 
    extends DocumentMetadata with IdFieldGenerator with ToJson {

    def withEvolutions(evolutions: Evolution*) =
      copy(evolutions = this.evolutions.withEvolutions(evolutions))
  }
}