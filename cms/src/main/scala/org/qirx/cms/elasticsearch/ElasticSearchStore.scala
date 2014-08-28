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

  private val stores = mutable.Map.empty[String, DocumentStore]

  private def storeFor(metaId: String) =
    stores.getOrElseUpdate(metaId, new DocumentStore(metaId))

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

  private class DocumentStore(metaId: String) {

    def typeEndpoint(typeName: String) = endpoint + "/" + indexName + "/" + typeName

    private val documentEndpoint = typeEndpoint(metaId)
    private val idMappingsEndpoint = typeEndpoint("id_mappings")

    private def documentPath = client.url(documentEndpoint)
    private def documentWithPath(path: String) = client.url(documentEndpoint + "/" + path)
    private val searchDocument = documentWithPath("_search")
    private val queryDocument = documentWithPath("_query")

    private def idMappingWithPath(path: String) = client.url(idMappingsEndpoint + "/" + path)

    private val IDS = "ids"

    def save(id: String, document: JsObject): Future[Unit] =
      getIndexId(id)
        .flatMap {
          case Some(indexId) => overrideDocument(indexId, document)
          case None => saveNewDocument(id, document)
        }

    def list(fieldSet: Set[String]): Future[Seq[JsObject]] = {

      var url = searchDocument
      if (fieldSet.nonEmpty)
        url = url.withQueryString(fieldSetAsQueryString(fieldSet))

      url.get.map(responseAsSeq andThen extractDocuments)
    }

    def get(id: String, fieldSet: Set[String]): Future[Option[JsObject]] =
      getIndexId(id).flatMap {
        case Some(indexId) =>
          documentWithPath(indexId)
            .withQueryString(fieldSetAsQueryString(fieldSet))
            .get
            .map(responseAsJsObject andThen extractDocument)
        case None => futureOfNone
      }

    def addId(id: String, newId: String): Future[Unit] =
      getIndexId(id).flatMap {
        case Some(indexId) =>
          idMappingWithPath(indexId + "/_update")
            .withQueryString(refresh)
            .post(
              obj(
                "script" -> "ctx._source.ids += id",
                "params" -> obj("id" -> id)

              )
            )
            .map(toUnit)
        case None => futureOfUnit
      }

    def deleteAll(): Future[Unit] =
      queryDocument
        .withBody(matchAllQuery)
        .withQueryString(refresh)
        .delete
        .map(toUnit)

    def delete(id: String): Future[Unit] =
      getIndexId(id).flatMap {
        case Some(indexId) =>
          documentWithPath(indexId)
            .withQueryString(refresh)
            .delete
            .map(toUnit)
        case None =>
          Future successful (())
      }

    def exists(id: String): Future[Boolean] =
      searchIdMappings(id).head.map(_.status == 200)

    private val toUnit: Any => Unit = _ => ()

    private def getIndexId(id: String): Future[Option[String]] =
      searchIdMappings(id)
        .get
        .map(responseAsSeq andThen (_.headOption) andThen extractId)

    private def searchIdMappings(id: String) =
      idMappingWithPath("_search").withBody(
        obj(
          "query" -> obj(
            "term" -> obj(IDS -> id)
          )
        )
      )

    private val responseAsSeq: WSResponse => Seq[JsObject] = {
      case response if response.status == 200 =>
        (response.json \ "hits" \ "hits").as[Seq[JsObject]]
      case response => Seq.empty
    }

    private val extractId: Option[JsObject] => Option[String] =
      _ map { o => (o \ "_id").as[String] }

    private def overrideDocument(indexId: String, document: JsObject) =
      documentWithPath(indexId)
        .withQueryString(refresh)
        .put(document)
        .map { _ => () }

    private def saveNewDocument(id: String, document: JsObject) =
      storeNewDocument(document)
        .flatMap(indexId => createIdMapping(indexId, id))
        .map(toUnit)

    private def storeNewDocument(document: JsObject) =
      documentPath
        .withQueryString(refresh)
        .post(document)
        .map(responseAsJsObject andThen (o => (o \ "_id").as[String]))

    private def createIdMapping(indexId: String, id: String) =
      idMappingWithPath(indexId)
        .withQueryString(refresh)
        .put(obj("ids" -> Seq(id)))

    private val responseAsJsObject: WSResponse => JsObject =
      _.json.as[JsObject]

    private val extractDocuments: Seq[JsObject] => Seq[JsObject] =
      _.map(extractDocument).flatten

    private val extractDocument: JsObject => Option[JsObject] =
      o => (o \ "_source").asOpt[JsObject]

    private val futureOfNone = Future successful None
    private val futureOfUnit = Future successful (())

    private def fieldSetAsQueryString(fieldSet: Set[String]) =
      "_source" -> fieldSet.map("document." + _).mkString(",")

    private val matchAllQuery =
      obj(
        "query" -> obj(
          "match_all" -> obj()
        )
      )

    private val refresh = "refresh" -> "true"

  }
}