package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject

class MemoryStore extends (Store ~> Future) {

  import Store._

  type Storage = mutable.Map[String, JsObject]
  private val store = mutable.Map.empty[String, Storage]

  private def storeFor(metaId: String) =
    store.getOrElseUpdate(metaId, mutable.LinkedHashMap.empty[String, JsObject])

  private val idMappings = mutable.Map.empty[String, mutable.Map[String, String]]

  private def idMappingsFor(metaId: String) =
    idMappings.getOrElseUpdate(metaId, mutable.Map.empty[String, String])

  private val unit = ()

  private def getActualId(id: String)(implicit store: Storage, idMappings: mutable.Map[String, String]): Option[String] =
    if (store contains id) Some(id)
    else idMappings.get(id).flatMap(getActualId)

  def transform[x] = {
    case List(metaId, fieldSet) =>
      val documents = storeFor(metaId).values.to[Seq]

      val documentsWithFields =
        if (fieldSet.isEmpty) documents
        else documents.map(filterFieldsWith(fieldSet))

      Future successful documentsWithFields

    case Save(metaId, id, document) =>
      storeFor(metaId) += (id -> document)
      Future successful unit

    case Get(metaId, id, fieldSet) =>
      val store = storeFor(metaId)
      val idMappings = idMappingsFor(metaId)
      val document = getActualId(id)(store, idMappings).flatMap(store.get)

      val documentWithFields =
        if (fieldSet.isEmpty) document
        else document.map(filterFieldsWith(fieldSet))

      Future successful documentWithFields

    case UpdateId(metaId, id, newId) =>
      val store = storeFor(metaId)
      val idMappings = idMappingsFor(metaId)

      val document = store.get(id)
      document.foreach { document =>
        store += (newId -> document)
        idMappings += (id -> newId)
        store -= id
      }

      Future successful unit

    case GetActualId(metaId, id) =>
      val store = storeFor(metaId)
      val idMappings = idMappingsFor(metaId)

      Future successful getActualId(id)(store, idMappings)

    case Delete(metaId, id) =>
      val store = storeFor(metaId)
      val idMappings = idMappingsFor(metaId)
      id.fold(ifEmpty = store.clear()) { id =>
        getActualId(id)(store, idMappings).foreach { id =>
          store -= id
        }
      }

      Future successful unit

    case Exists(metaId, id) =>
      val store = storeFor(metaId)
      val idMappings = idMappingsFor(metaId)

      Future successful getActualId(id)(store, idMappings).map(store.contains).getOrElse(false)
  }

  private def filterFieldsWith(fieldSet: Set[String]): JsObject => JsObject = { document =>
    val filteredFields = document.fields.filter {
      case (key, _) => fieldSet contains key
    }
    JsObject(filteredFields)
  }
}