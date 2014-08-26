package org.qirx.cms.testing

import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Index
import scala.concurrent.Future
import play.api.mvc.Results
import scala.collection.mutable
import play.api.libs.json.JsObject

class MemoryIndex extends (Index ~> Future) {

  import Index._

  private val indexes = mutable.Map.empty[String, DocumentIndex]

  private def indexFor(metaId: String) =
    indexes.getOrElseUpdate(metaId, new DocumentIndex)

  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      Future successful indexFor(metaId).get(id, fieldSet)
      
    case List(metaId, fieldSet) =>
      Future successful indexFor(metaId).list(fieldSet)
      
    case Put(metaId, id, document) =>
      Future successful indexFor(metaId).put(id, document)
      
    case Delete(metaId, id) =>
      Future successful indexFor(metaId).delete(id)
      
    case DeleteAll(metaId) =>
      Future successful indexFor(metaId).deleteAll()
      
    case UpdateId(metaId, id, newId) =>
      Future successful indexFor(metaId).updateId(id, newId)

    case Search(request, remainingPathSegments) =>
      Future successful Results.Accepted
  }

  private class DocumentIndex {
    private val index = mutable.LinkedHashMap.empty[String, JsObject]

    def list(fieldSet: Set[String]): Seq[JsObject] = {
      val documents = index.values.to[Seq]

      val documentsWithFields =
        if (fieldSet.isEmpty) documents
        else documents.map(filterFieldsWith(fieldSet))

      documentsWithFields
    }

    def put(id: String, document: JsObject): Unit =
      index += (id -> document)

    def get(id: String, fieldSet: Set[String]): Option[JsObject] = {

      val document = index.get(id)

      val documentWithFields =
        if (fieldSet.isEmpty) document
        else document.map(filterFieldsWith(fieldSet))

      documentWithFields
    }

    def updateId(id: String, newId: String): Unit = {
      val document = index.get(id)
      document.foreach { document =>
        index += (newId -> document)
        index -= id
      }
    }

    def deleteAll(): Unit = index.clear()
    
    def delete(id: String): Unit =
      index -= id

    private def filterFieldsWith(fieldSet: Set[String]): JsObject => JsObject = { document =>
      val filteredFields = document.fields.filter {
        case (key, _) => fieldSet contains key
      }
      JsObject(filteredFields)
    }
  }
}