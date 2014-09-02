package org.qirx.cms.elasticsearch

import org.qirx.cms.construction.{ Index => CmsIndex }
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
import org.qirx.cms.metadata.DocumentMetadata
import scala.concurrent.duration._
import scala.concurrent.Await

class Index(
  documentMetadata: Seq[DocumentMetadata with DocumentMapping],
  endpoint: String,
  indexName: String,
  client: WSClient)(implicit ec: ExecutionContext) extends (CmsIndex ~> Future) {

  deleteIndex()
  createIndexWithMappings()

  lazy val documentMetadataMap = 
    documentMetadata.map(document => document.id -> document).toMap
  
  private val indexes = mutable.Map.empty[String, DocumentStore]

  private def indexFor(metaId: String) =
    indexes.getOrElseUpdate(metaId,
      new DocumentStore(endpoint, indexName, metaId, client))

  import CmsIndex._

  def transform[x] = {
    case List(metaId, fieldSet) =>
      indexFor(metaId).list(fieldSet)

    case Put(metaId, id, document) =>
      val meta = documentMetadataMap.get(metaId)
      val transformedDocument = 
        meta.map(_.transform(document)).getOrElse(document)
        
      indexFor(metaId).save(id, transformedDocument)

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

  private def deleteIndex() = {
    val response =
      client
        .url(endpoint + "/" + indexName)
        .delete

    Await.result(response, 2.seconds)
  }

  private def createIndexWithMappings() = {
    val mappings =
      documentMetadata.foldLeft(obj()) { (o, documentMetadata) =>
        o ++ obj(documentMetadata.id -> documentMetadata.mapping)
      }

    val response =
      client
        .url(endpoint + "/" + indexName)
        .put(obj("mappings" -> mappings))
        .map {
          case response if response.status == 200 => None
          case response => Some(response.json)
        }

    val result = Await.result(response, 2.seconds)
    result.foreach { json =>
      sys.error(json.toString)
    }
  }

}