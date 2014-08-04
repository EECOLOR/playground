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
import org.qirx.cms.construction.Create
import org.qirx.cms.construction.Update
import org.qirx.cms.construction.List

class TestStore extends (Store ~> Future) {

  val storage = mutable.Map.empty[String, ListBuffer[JsObject]]

  val idMappings = mutable.Map.empty[String, String]

  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      val store = storage.getOrElseUpdate(metaId, mutable.ListBuffer.empty)
      def getId(id: String): String =
        idMappings.get(id).map(getId).getOrElse(id)

      val actualId = getId(id)
      val obj = store.find(obj => (obj \ "id").as[String] == actualId)
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

    case Create(metaId, id, json) =>
      val store = storage.getOrElseUpdate(metaId, mutable.ListBuffer.empty)
      store += json ++ obj("id" -> id)

      Future.successful(())

    case Update(metaId, id, oldObj, newObj, fieldSet) =>
      val store = storage.getOrElseUpdate(metaId, mutable.ListBuffer.empty)

      val index = store.indexOf(oldObj)
      val oldId = (oldObj \ "id").as[String]
      val newId = (newObj \ "id").asOpt[String]

      newId.foreach(idMappings.update(oldId, _))

      val actualId = newId.getOrElse(oldId)

      val filteredObj =
        if (fieldSet.isEmpty) newObj + ("id" -> JsString(actualId))
        else {
          val filteredFields = newObj.fields.filter {
            case (key, _) => fieldSet contains key
          }
          oldObj ++ JsObject(filteredFields)
        }
      store.update(index, filteredObj)

      Future.successful(())

    case List(metaId, fields) =>
      val documents = storage.getOrElseUpdate(metaId, mutable.ListBuffer.empty)
      val documentsWithFields =
        if (fields.isEmpty) documents
        else
          documents.map { document =>
            JsObject(document.fields.filter {
              case (key, _) => fields.contains(key)
            })
          }
      Future.successful(documentsWithFields)
  }
}
