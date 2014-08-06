package testUtils

import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.collection.mutable.ListBuffer
import play.api.libs.json.JsString
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Get
import org.qirx.cms.construction.Save
import org.qirx.cms.construction.SaveIdReference
import org.qirx.cms.construction.List
import org.qirx.cms.construction.Delete

class TestStore extends (Store ~> Future) {

  val storage = mutable.Map.empty[String, mutable.Map[String, JsObject]]

  val idMappings = mutable.Map.empty[String, String]

  def storeFor(metaId:String) = storage.getOrElseUpdate(metaId, mutable.Map.empty)
  
  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      val store = storeFor(metaId)
      def getId(id: String): String =
        idMappings.get(id).map(getId).getOrElse(id)

      val actualId = getId(id)
      val obj = store.get(actualId)
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

    case Save(metaId, id, json) =>
      val store = storeFor(metaId)
      store += (id -> json)

      Future.successful(())

    case SaveIdReference(metaId, id, newId) =>
      newId.foreach(newId => idMappings += (id -> newId))
      
      Future.successful(())
      
    case Delete(metaId, id) =>
      val store = storeFor(metaId)
      store -= id
      
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
}
