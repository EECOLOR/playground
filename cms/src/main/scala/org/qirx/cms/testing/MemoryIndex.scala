package org.qirx.cms.testing

import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Index
import scala.concurrent.Future
import play.api.mvc.Results
import scala.collection.mutable

/**
 * This implementation is not thread safe
 */
class MemoryIndex extends (Index ~> Future) {

  import Index._

  private val indexes = mutable.Map.empty[String, MemoryDocumentStore]

  private def indexFor(metaId: String) =
    indexes.getOrElseUpdate(metaId, new MemoryDocumentStore)

  def transform[x] = {
    case Get(metaId, id, fieldSet) =>
      Future successful indexFor(metaId).get(id, fieldSet)
      
    case List(metaId, fieldSet) =>
      Future successful indexFor(metaId).list(fieldSet)
      
    case Put(metaId, id, document) =>
      Future successful indexFor(metaId).save(id, document)
      
    case Exists(metaId, id) =>
      Future successful indexFor(metaId).exists(id)
      
    case Delete(metaId, id) =>
      Future successful indexFor(metaId).delete(id)
      
    case DeleteAll(metaId) =>
      Future successful indexFor(metaId).deleteAll()
      
    case AddId(metaId, id, newId) =>
      Future successful indexFor(metaId).addId(id, newId)

    case Search(request, remainingPathSegments) =>
      Future successful Results.Accepted
      
    case Count(request, remainingPathSegments) =>
      Future successful Results.Accepted
  }
}