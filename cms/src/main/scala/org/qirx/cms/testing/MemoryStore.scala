package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable

/**
 * This implementation is not thread safe
 */
class MemoryStore extends (Store ~> Future) {

  import Store._

  private val stores = mutable.Map.empty[String, MemoryDocumentStore]

  private def storeFor(metaId: String) =
    stores.getOrElseUpdate(metaId, new MemoryDocumentStore)

  def transform[x] = {
    case List(metaId, fieldSet) =>
      Future successful storeFor(metaId).list(fieldSet)

    case Save(metaId, id, document) =>
      Future successful storeFor(metaId).save(id, document)

    case Get(metaId, id, fieldSet) =>
      Future successful storeFor(metaId).get(id, fieldSet)

    case AddId(metaId, id, newId) =>
      Future successful storeFor(metaId).addId(id, newId)

    case Delete(metaId, id) =>
      Future successful storeFor(metaId).delete(id)

    case DeleteAll(metaId) =>
      Future successful storeFor(metaId).deleteAll()

    case Exists(metaId, id) =>
      Future successful storeFor(metaId).exists(id)
  }
}