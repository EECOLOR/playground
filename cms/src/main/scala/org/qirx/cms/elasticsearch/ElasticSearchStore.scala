package org.qirx.cms.elasticsearch

import org.qirx.cms.construction.Store
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

class ElasticSearchStore(endpoint: String, indexName: String, client: WSClient)(implicit ec: ExecutionContext) extends (Store ~> Future) {

  private val stores = mutable.Map.empty[String, ElasticSearchDocumentStore]

  private def storeFor(metaId: String) =
    stores.getOrElseUpdate(metaId,
      new ElasticSearchDocumentStore(endpoint, indexName, metaId, client))

  import Store._

  def transform[x] = {
    case List(metaId, fieldSet) =>
      storeFor(metaId).list(fieldSet)

    case Save(metaId, id, document) =>
      storeFor(metaId).save(id, document)

    case Get(metaId, id, fieldSet) =>
      storeFor(metaId).get(id, fieldSet)

    case AddId(metaId, id, newId) =>
      storeFor(metaId).addId(id, newId)

    case Delete(metaId, id) =>
      storeFor(metaId).delete(id)

    case DeleteAll(metaId) =>
      storeFor(metaId).deleteAll()

    case Exists(metaId, id) =>
      storeFor(metaId).exists(id)
  }
}