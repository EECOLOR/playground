package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject

object MemoryStore extends (Store ~> Future) {

  import Store._
  
  private val metaStore = mutable.Map.empty[String, mutable.Map[String, JsObject]]
  
  def storeFor(metaId:String) = 
    metaStore.getOrElseUpdate(metaId, mutable.LinkedHashMap.empty[String, JsObject])
  
  private val unit = ()
  
  def transform[x] = {
    case List(metaId, fieldSet) =>
      Future successful storeFor(metaId).values.to[Seq]
      
    case Save(metaId, id, document) =>
      storeFor(metaId) += (id -> document)
      Future successful unit
    
    case Get(metaId, id, fieldSet) => 
      Future successful storeFor(metaId).get(id)
      
    case SaveIdReference(metaId, id, newId) => ???
    case GetActualId(metaId, id) => ???
    case Delete(metaId, id) => ???
    case Exists(metaId, id) => ???
  }
}