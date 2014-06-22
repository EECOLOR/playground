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

class TestSystem extends System {

  val storage = mutable.Map.empty[String, ListBuffer[JsObject]]

  def performAction[T](action: Action[T]): Future[T] =
    action match {
      case Get(document, id, fieldSet) =>
        println("fieldset is ignored")
        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)
        val obj = store.find(obj => (obj \ "id").as[String] == id)
        Future.successful(obj)

      case Create(document, json) =>
        val id = document.idGenerator.generateFor(json)

        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)
        store += json ++ obj("id" -> id)

        Future.successful(id)
      case Update(document, oldObj, newObj, fieldSet) =>
        println("fieldset is ignored")

        val store = storage.getOrElseUpdate(document.id, mutable.ListBuffer.empty)

        val index = store.indexOf(oldObj)
        //val oldId = oldObj \ "id"
        //val newId = newObj \ "id"
        store.update(index, oldObj ++ newObj)

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