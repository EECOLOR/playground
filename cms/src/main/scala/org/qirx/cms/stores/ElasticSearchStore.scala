package org.qirx.cms.stores

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

class ElasticSearchStore(endpoint: String, client: WSClient)(implicit ec: ExecutionContext) extends (Store ~> Future) {

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

    case UpdateId(metaId, id, newId) =>
      storeFor(metaId).updateId(id, newId)

    case GetActualId(metaId, id) =>
      storeFor(metaId).getActualId(id)

    case Delete(metaId, id) =>
      storeFor(metaId).delete(id)

    case DeleteAll(metaId) =>
      storeFor(metaId).deleteAll

    case Exists(metaId, id) =>
      storeFor(metaId).exists(id)
  }

  private class DocumentStore(metaId: String) {

    private val currentEndpoint = endpoint + "/" + metaId

    private def path(path: String) = client.url(currentEndpoint + "/" + path)
    private def withId(id: String) = path(id)
    private val search = path("_search")
    private val query = path("_query")

    def save(id: String, document: JsObject): Future[Unit] = {
      val wrappedDocument =
        obj(
          "document" -> document,
          "alternativeIds" -> arr()
        )

      rawSave(id, wrappedDocument)
    }

    def list(fieldSet: Set[String]): Future[Seq[JsObject]] = {

      var url = search
      if (fieldSet.nonEmpty)
        url = url.withQueryString(fieldSetAsQueryString(fieldSet))

      url.get.map(responseAsSeq andThen extractDocuments)
    }

    def get(id: String, fieldSet: Set[String]): Future[Option[JsObject]] =
      rawGet(id, fieldSet).map(extractDocument)

    def updateId(id: String, newId: String): Future[Unit] = {
      val updateDocumentId = updateId(id, newId, _: Option[JsObject])
      rawGet(id, Set.empty).flatMap(updateDocumentId)
    }

    def getActualId(id: String): Future[Option[String]] =
      rawGet(id, Set.empty).map(extractId)

    def deleteAll: Future[Unit] =
      query
        .withBody(matchAllQuery)
        .withQueryString(refresh)
        .delete
        .map { _ => () }

    def delete(id: String): Future[Unit] =
      withId(id)
        .withQueryString(refresh)
        .delete
        .map { _ => () }

    def exists(id: String): Future[Boolean] =
      rawGet(id, Set.empty).map { _.isDefined }

    private def rawSave(id: String, document: JsObject): Future[Unit] =
      withId(id)
        .withQueryString(refresh)
        .put(document)
        .map { _ => () }

    private def fieldSetAsQueryString(fieldSet: Set[String]) =
      "_source" -> fieldSet.map("document." + _).mkString(",")

    private def findById(id: String) =
      search
        .withBody(
          obj(
            "query" -> obj(
              "bool" -> obj(
                "should" -> arr(
                  obj("term" -> obj("_id" -> id)),
                  obj("term" -> obj("alternativeIds" -> id))
                )
              )
            )
          )
        )

    private def rawGet(id: String, fieldSet: Set[String] = Set.empty) = {
      var url = findById(id)
      if (fieldSet.nonEmpty)
        url = url.withQueryString(fieldSetAsQueryString(fieldSet))

      url.get.map(responseAsSeq).map(_.headOption)
    }

    private val responseAsSeq: WSResponse => Seq[JsObject] =
      response => (response.json \ "hits" \ "hits").as[Seq[JsObject]]

    private val extractId: Option[JsObject] => Option[String] =
      _ map { o => (o \ "_id").as[String] }

    private val extractDocument: Option[JsObject] => Option[JsObject] =
      _ map extractDocumentFromObject

    private val extractDocuments: Seq[JsObject] => Seq[JsObject] =
      _ map extractDocumentFromObject

    private def extractDocumentFromObject(o: JsObject) =
      (o \ "_source" \ "document").asOpt[JsObject].getOrElse(obj())

    private def updateId(id: String, newId: String, document: Option[JsObject]) =
      document
        .map { document =>
          val existingAlternativeIds = (document \ "_source" \ "alternativeIds").as[JsArray]
          val alternativeIds = existingAlternativeIds :+ JsString(id)
          val newDocument =
            obj(
              "document" -> extractDocumentFromObject(document),
              "alternativeIds" -> alternativeIds
            )

          for {
            _ <- rawSave(newId, newDocument)
            _ <- delete(id)
          } yield ()
        }
        .getOrElse(Future successful (()))

    private val matchAllQuery =
      obj(
        "query" -> obj(
          "match_all" -> obj()
        )
      )

    private val refresh = "refresh" -> "true"

  }
}