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

class ElasticSearchIndex(endpoint: String, client: WSClient)(implicit ec: ExecutionContext) extends (Index ~> Future) {

  private val indexes = mutable.Map.empty[String, DocumentIndex]

  private def indexFor(metaId: String) =
    indexes.getOrElseUpdate(metaId, new DocumentIndex(metaId))

  import Index._

  def transform[x] = {
    case List(metaId, fieldSet) =>
      indexFor(metaId).list(fieldSet)

    case Put(metaId, id, document) =>
      indexFor(metaId).put(id, document)

    case Get(metaId, id, fieldSet) =>
      indexFor(metaId).get(id, fieldSet)

    case AddId(metaId, id, newId) =>
      indexFor(metaId).updateId(id, newId)

    case Delete(metaId, id) =>
      indexFor(metaId).delete(id)

    case DeleteAll(metaId) =>
      indexFor(metaId).deleteAll()

    case Search(request, remainingPath) =>
      ???
  }

  private class DocumentIndex(metaId: String) {

    private val currentEndpoint = endpoint + "/" + metaId

    private def path(path: String) = client.url(currentEndpoint + "/" + path)
    private def withId(id: String) = path(id)
    private val search = path("_search")
    private val query = path("_query")

    def put(id: String, document: JsObject): Future[Unit] =
      withId(id)
        .withQueryString(refresh)
        .put(document)
        .map { _ => () }

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

    def deleteAll(): Future[Unit] =
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

    private def fieldSetAsQueryString(fieldSet: Set[String]) =
      "_source" -> fieldSet.mkString(",")

    private def findById(id: String) =
      search
        .withBody(
          obj(
            "query" -> obj(
              "term" -> obj("_id" -> id)
            )
          )
        )

    private def rawGet(id: String, fieldSet: Set[String] = Set.empty) = {
      var url = findById(id)
      if (fieldSet.nonEmpty)
        url = url.withQueryString(fieldSetAsQueryString(fieldSet))

      url.get.map(responseAsSeq).map(_.headOption)
    }

    private val responseAsSeq: WSResponse => Seq[JsObject] = {
      case response if response.status == 404 => Seq.empty
      case response =>
        (response.json \ "hits" \ "hits").as[Seq[JsObject]]
    }

    private val extractId: Option[JsObject] => Option[String] =
      _ map { o => (o \ "_id").as[String] }

    private val extractDocument: Option[JsObject] => Option[JsObject] =
      _ map extractDocumentFromObject

    private val extractDocuments: Seq[JsObject] => Seq[JsObject] =
      _ map extractDocumentFromObject

    private def extractDocumentFromObject(o: JsObject) =
      (o \ "_source").asOpt[JsObject].getOrElse(obj())

    private def updateId(id: String, newId: String, document: Option[JsObject]) =
      document
        .map { document =>
          for {
            _ <- put(newId, extractDocumentFromObject(document))
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