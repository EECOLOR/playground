package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future

object MemoryStore extends (Store ~> Future) {

  import Store._
  
  def transform[x] = {
    case List(metaId, fieldSet) => ???
    case Get(metaId, id, fieldSet) => ???
    case Save(metaId, id, document) => ???
    case SaveIdReference(metaId, id, newId) => ???
    case GetActualId(metaId, id) => ???
    case Delete(metaId, id) => ???
    case Exists(metaId, id) => ???
  }
}