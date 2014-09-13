package org.qirx.cms.elasticsearch

import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class IndexStore(
  endpoint: String,
  indexName: String,
  metaId: String,
  client: WSClient)(implicit ec: ExecutionContext) {

  private val documentEndpoint =
    s"$endpoint/$indexName/$metaId"

  private val idMappingsEndpoint =
    s"$endpoint/$indexName/${metaId}_id_mappings"

  private def url(url: String) = client.url(url)

  def save(id: String, document: JsObject): Future[Unit] =
    getIndexId(id)
      .flatMap {
        case Some(indexId) => overrideDocument(indexId, document)
        case None => saveNewDocument(id, document)
      }

  def list(fieldSet: Set[String]): Future[Seq[JsObject]] = {

    var searchUrl = url(documentEndpoint + "/_search")
    if (fieldSet.nonEmpty)
      searchUrl = searchUrl.withQueryString(fieldSetAsQueryString(fieldSet))

    searchUrl.get.map(responseAsSeq andThen extractDocuments)
  }

  def get(id: String, fieldSet: Set[String]): Future[Option[JsObject]] =
    getIndexId(id).flatMap {
      case Some(indexId) =>
        url(documentEndpoint + "/" + indexId)
          .withQueryString(fieldSetAsQueryString(fieldSet))
          .get
          .map(responseAsJsObject andThen extractDocument)
      case None => futureOfNone
    }

  def addId(id: String, newId: String): Future[Unit] =
    getIndexId(id).flatMap {
      case Some(indexId) =>
        url(idMappingsEndpoint + "/" + indexId + "/_update")
          .withQueryString(refresh)
          .post(
            obj(
              "script" -> "ctx._source.ids += id",
              "params" -> obj("id" -> newId)

            )
          )
          .map(toUnit)
      case None => futureOfUnit
    }

  def deleteAll(): Future[Unit] = {
    val deleteDocuments =
      url(documentEndpoint + "/_query")
        .withBody(matchAllQuery)
        .withQueryString(refresh)
        .delete

    val deleteIdMappings =
      url(idMappingsEndpoint + "/_query")
        .withBody(matchAllQuery)
        .withQueryString(refresh)
        .delete

    Future.sequence(Seq(deleteDocuments, deleteIdMappings)).map(toUnit)
  }

  def delete(id: String): Future[Unit] =
    getIndexId(id).flatMap {
      case Some(indexId) =>
        val removeDocument =
          url(documentEndpoint + "/" + indexId)
            .withQueryString(refresh)
            .delete

        val removeIdMapping =
          url(idMappingsEndpoint + "/" + indexId)
            .withQueryString(refresh)
            .delete

        Future.sequence(Seq(removeDocument, removeIdMapping)).map(toUnit)
      case None =>
        println("delete", id)
        Future successful (())
    }

  def exists(id: String): Future[Boolean] =
    url(idMappingsEndpoint + "/_count")
      .withBody(searchIdQuery(id))
      .get
      .map(r => (r.json \ "count").as[Int] > 0)

  private val toUnit: Any => Unit = _ => ()

  private def getIndexId(id: String): Future[Option[String]] =
    searchIdMappings(id)
      .get
      .map(responseAsSeq andThen (_.headOption) andThen extractId)

  private def searchIdMappings(id: String) =
    url(idMappingsEndpoint + "/_search").withBody(searchIdQuery(id))

  private val responseAsSeq: WSResponse => Seq[JsObject] = {
    case response if response.status == 200 =>
      (response.json \ "hits" \ "hits").as[Seq[JsObject]]
    case response => Seq.empty
  }

  private val extractId: Option[JsObject] => Option[String] =
    _ map { o => (o \ "_id").as[String] }

  private def overrideDocument(indexId: String, document: JsObject) =
    url(documentEndpoint + "/" + indexId)
      .withQueryString(refresh)
      .put(document)
      .map { _ => () }

  private def saveNewDocument(id: String, document: JsObject) =
    storeNewDocument(document)
      .flatMap(indexId => createIdMapping(indexId, id))
      .map(toUnit)

  private def storeNewDocument(document: JsObject) =
    url(documentEndpoint)
      .withQueryString(refresh)
      .post(document)
      .map(responseAsJsObject andThen (o => (o \ "_id").as[String]))

  private def createIdMapping(indexId: String, id: String) =
    url(idMappingsEndpoint + "/" + indexId)
      .withQueryString(refresh)
      .put(obj("ids" -> Seq(id)))

  private def searchIdQuery(id: String) =
    obj(
      "query" -> obj(
        "term" -> obj("ids" -> id)
      )
    )

  private val responseAsJsObject: WSResponse => JsObject =
    _.json.as[JsObject]

  private val extractDocuments: Seq[JsObject] => Seq[JsObject] =
    _.map(extractDocument).flatten

  private val extractDocument: JsObject => Option[JsObject] =
    o => (o \ "_source").asOpt[JsObject]

  private val futureOfNone = Future successful None
  private val futureOfUnit = Future successful (())

  private def fieldSetAsQueryString(fieldSet: Set[String]) =
    "_source" -> fieldSet.mkString(",")

  private val matchAllQuery =
    obj(
      "query" -> obj(
        "match_all" -> obj()
      )
    )

  private val refresh = "refresh" -> "true"

}