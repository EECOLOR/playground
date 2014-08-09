package testUtils

import org.qirx.cms.construction.Index
import org.qirx.cms.construction.Index._
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.mvc.Results._
import play.api.libs.json.Json.obj

class TestIndex extends (Index ~> Future) {

  val storage = mutable.Map.empty[String, mutable.Map[String, JsObject]]

  def storeFor(metaId: String) = storage.getOrElseUpdate(metaId, mutable.LinkedHashMap.empty)

  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      val store = storeFor(metaId)

      val obj = store.get(id)
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

    case List(metaId, fields) =>
      val store = storeFor(metaId)
      val documents = store.values
      val documentsWithFields =
        if (fields.isEmpty) documents
        else documents.map { document =>
          JsObject(document.fields.filter {
            case (key, _) => fields.contains(key)
          })
        }

      Future.successful(documentsWithFields.toSeq)

    case Put(metaId, id, document) =>
      val store = storeFor(metaId)
      store += (id -> document)

      Future.successful(())

    case Delete(metaId, id) =>
      val store = storeFor(metaId)
      id.fold(ifEmpty = store.clear())(store -= _)

      Future.successful(())

    case Search(request, remainingPathSegments) =>
      val searchResult =
        obj(
          "info" -> s"Response from test index to search at `${remainingPathSegments.mkString}`"
        )

      Future.successful(Ok(searchResult))

  }
}