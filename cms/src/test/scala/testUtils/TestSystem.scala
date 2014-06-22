package testUtils

import org.qirx.cms.system.System
import org.qirx.cms.system.Action
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.system.List
import scala.collection.mutable.ListBuffer
import org.qirx.cms.system.Create
import org.qirx.cms.system.Update
import org.qirx.cms.system.Get
import play.api.libs.json.JsString

class TestSystem extends System {

  val storage = mutable.Map.empty[String, ListBuffer[JsObject]]

  val idMappings = mutable.Map.empty[String, String]
  
  def performAction[T](action: Action[T]): Future[T] =
    action match {
      case Get(document, id, fieldSet) =>
        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)
        def getId(id:String):String = 
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

      case Create(document, json) =>
        val id = document.idGenerator.generateFor(json)

        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)
        store += json ++ obj("id" -> id)

        Future.successful(id)
      case Update(document, oldObj, newObj, fieldSet) =>
        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)

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
      case List(document, fields) =>
        val documents = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)
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