package org.qirx.cms.elasticsearch

import org.qirx.cms.construction.Index
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import java.net.URI
import play.api.libs.ws.WSClient
import scala.concurrent.ExecutionContext
import play.api.libs.json.JsArray
import play.api.libs.ws.WSRequestHolder
import play.api.libs.json.JsString
import play.api.libs.ws.WSResponse

class ElasticSearchIndex(endpoint: String, indexName:String, client: WSClient)(implicit ec: ExecutionContext) extends (Index ~> Future) {

  private val indexes = mutable.Map.empty[String, ElasticSearchDocumentStore]

  private def indexFor(metaId: String) =
    indexes.getOrElseUpdate(metaId,
      new ElasticSearchDocumentStore(endpoint, indexName, metaId, client))

  import Index._

  def transform[x] = {
    case List(metaId, fieldSet) =>
      indexFor(metaId).list(fieldSet)

    case Put(metaId, id, document) =>
      indexFor(metaId).save(id, document)

    case Get(metaId, id, fieldSet) =>
      indexFor(metaId).get(id, fieldSet)

    case AddId(metaId, id, newId) =>
      indexFor(metaId).addId(id, newId)

    case Exists(metaId, id) =>
      indexFor(metaId).exists(id)
      
    case Delete(metaId, id) =>
      indexFor(metaId).delete(id)

    case DeleteAll(metaId) =>
      indexFor(metaId).deleteAll()

    case Search(request, remainingPath) =>
      ???
  }
}