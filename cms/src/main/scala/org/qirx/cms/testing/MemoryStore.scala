package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject

class MemoryStore extends (Store ~> Future) {

  import Store._

  private val stores = mutable.Map.empty[String, DocumentStore]

  private def storeFor(metaId: String) =
    stores.getOrElseUpdate(metaId, new DocumentStore)

  def transform[x] = {
    case List(metaId, fieldSet) =>
      Future successful storeFor(metaId).list(fieldSet)

    case Save(metaId, id, document) =>
      Future successful storeFor(metaId).save(id, document)

    case Get(metaId, id, fieldSet) =>
      Future successful storeFor(metaId).get(id, fieldSet)

    case UpdateId(metaId, id, newId) =>
      Future successful storeFor(metaId).updateId(id, newId)

    case GetActualId(metaId, id) =>
      Future successful storeFor(metaId).getActualId(id)

    case Delete(metaId, id) =>
      Future successful storeFor(metaId).delete(id)

    case Exists(metaId, id) =>
      Future successful storeFor(metaId).exists(id)
  }

  private class DocumentStore {
    private val store = mutable.LinkedHashMap.empty[String, JsObject]
    private val idMappings = mutable.Map.empty[String, String]

    def list(fieldSet: Set[String]): Seq[JsObject] = {
      val documents = store.values.to[Seq]

      val documentsWithFields =
        if (fieldSet.isEmpty) documents
        else documents.map(filterFieldsWith(fieldSet))

      documentsWithFields
    }

    def save(id: String, document: JsObject): Unit =
      store += (id -> document)

    def get(id: String, fieldSet: Set[String]): Option[JsObject] = {

      val document = getActualId(id).flatMap(store.get)

      val documentWithFields =
        if (fieldSet.isEmpty) document
        else document.map(filterFieldsWith(fieldSet))

      documentWithFields
    }

    def updateId(id: String, newId: String): Unit = {
      val document = store.get(id)
      document.foreach { document =>
        store += (newId -> document)
        idMappings += (id -> newId)
        store -= id
      }
    }

    def getActualId(id: String): Option[String] =
      if (store contains id) Some(id)
      else idMappings.get(id).flatMap(getActualId)

    def delete(id: Option[String]): Unit =
      id.flatMap(getActualId).fold(ifEmpty = store.clear()) { id =>
        store -= id
      }

    def exists(id: String): Boolean =
      getActualId(id).map(store.contains).getOrElse(false)

    private def filterFieldsWith(fieldSet: Set[String]): JsObject => JsObject = { document =>
      val filteredFields = document.fields.filter {
        case (key, _) => fieldSet contains key
      }
      JsObject(filteredFields)
    }
  }
}