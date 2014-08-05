package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata

sealed trait Store[T]
case class List(metaId: String, fieldSet: Set[String]) extends Store[Seq[JsObject]]
case class Get(metaId: String, id: String, fieldSet: Set[String]) extends Store[Option[JsObject]]
case class Save(metaId: String, id:String/* ReservedId */, document: JsObject) extends Store[Unit /* String --> id */]
case class SaveIdReference(metaId:String, id:String, newId:Option[String]) extends Store[Unit]
case class Delete(metaId: String, id:String) extends Store[Unit]
//case class Update(metaId: String, id: String, oldDocument: JsObject, newDocument: JsObject, fieldSet:Set[String]) extends Store[Unit]
//case class GetReservedId(idGenerator:DocumentIdGenerator) extends Store[Store.ReservedId]
/*
object Store {
  case class ReservedId(reservationId:String)
}
*/