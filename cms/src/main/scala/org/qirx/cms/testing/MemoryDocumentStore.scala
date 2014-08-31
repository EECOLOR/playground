package org.qirx.cms.testing

import scala.collection.mutable
import play.api.libs.json.JsObject
import java.util.UUID

/**
 * This implementation is not thread safe
 */
class MemoryDocumentStore {
  private val store = mutable.LinkedHashMap.empty[String, JsObject]
  private val idMappings = mutable.Map.empty[String, String]

  private def newId = UUID.randomUUID.toString

  def save(id: String, document: JsObject): Unit = {
    val actualId = idMappings.get(id).getOrElse(newId)
    store += (actualId -> document)
    idMappings += (id -> actualId)
  }

  def list(fieldSet: Set[String]): Seq[JsObject] = {
    val documents = store.values.to[Seq]

    val documentsWithFields =
      if (fieldSet.isEmpty) documents
      else documents.map(filterFieldsWith(fieldSet))

    documentsWithFields
  }

  def get(id: String, fieldSet: Set[String]): Option[JsObject] = {

    val document = idMappings.get(id).flatMap(store.get)

    val documentWithFields =
      if (fieldSet.isEmpty) document
      else document.map(filterFieldsWith(fieldSet))

    documentWithFields
  }

  def addId(id: String, newId: String): Unit =
    idMappings.get(id).foreach { actualId =>
      idMappings += (newId -> actualId)
    }

  def deleteAll(): Unit = {
    store.clear()
    idMappings.clear()
  }

  def delete(id: String): Unit =
    idMappings.get(id).foreach { id =>
      store -= id
      val oldIds =
        idMappings.collect {
          case (mappedId, actualId) if actualId == id =>
            mappedId
        }
      oldIds.foreach(idMappings.remove)
    }

  def exists(id: String): Boolean =
    idMappings.get(id).map(store.contains).getOrElse(false)

  private def filterFieldsWith(fieldSet: Set[String]): JsObject => JsObject =
    { document =>
      val filteredFields = document.fields.filter {
        case (key, _) => fieldSet contains key
      }
      JsObject(filteredFields)
    }
}