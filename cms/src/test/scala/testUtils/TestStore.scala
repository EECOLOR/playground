package testUtils

import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.collection.mutable.ListBuffer
import play.api.libs.json.JsString
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Store._
import org.qirx.cms.testing.MemoryStore

class TestStore extends MemoryStore {

  /*
  type Storage = mutable.Map[String, JsObject]
  val storage = mutable.Map.empty[String, Storage]

  val idMappings = mutable.Map.empty[String, String]

  def storeFor(metaId: String) = storage.getOrElseUpdate(metaId, mutable.LinkedHashMap.empty)

  def getActualId(id: String)(implicit store:Storage): Option[String] =
    if (store contains id) Some(id)
    else idMappings.get(id).flatMap(getActualId)

  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      val store = storeFor(metaId)

      val actualId = getActualId(id)(store)
      val obj = actualId.flatMap(store.get)
      val filteredObj =
        if (fieldSet.isEmpty) obj
        else obj.map { obj =>
          val filteredFields =
            obj.fields.filter {
              case (key, _) => fieldSet contains key
            }
          JsObject(filteredFields)
        }
      Future.successful(filteredObj)

    case Exists(metaId, id) =>
      val store = storeFor(metaId)
      val exists = store.contains(id)

      Future.successful(exists)

    case Save(metaId, id, json) =>
      val store = storeFor(metaId)
      store += (id -> json)

      Future.successful(())

    case UpdateId(metaId, id, newId) =>
      idMappings += (id -> newId)

      Future.successful(())

    case GetActualId(metaId, id) =>
      val store = storeFor(metaId)
      Future.successful(getActualId(id)(store))

    case Delete(metaId, id) =>
      val store = storeFor(metaId)
      id.fold(ifEmpty = store.clear())(store -= _)

      Future.successful(())

    case List(metaId, fields) =>
      val store = storeFor(metaId)
      val documents = store.values
      val documentsWithFields =
        if (fields.isEmpty) documents
        else
          documents.map { document =>
            JsObject(document.fields.filter {
              case (key, _) => fields.contains(key)
            })
          }
      Future.successful(documentsWithFields.toSeq)
  }
  */
}
