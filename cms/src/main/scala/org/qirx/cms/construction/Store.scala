package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata

sealed trait Store[T]
object Store {
  case class List(metaId: String, fieldSet: Set[String] = Set.empty) extends Store[Seq[JsObject]]
  case class Get(metaId: String, id: String, fieldSet: Set[String] = Set.empty) extends Store[Option[JsObject]]
  case class Save(metaId: String, id: String, document: JsObject) extends Store[Unit]
  case class UpdateId(metaId: String, id: String, newId: String) extends Store[Unit]
  case class GetActualId(metaId:String, id:String) extends Store[Option[String]]
  case class Delete(metaId: String, id: Option[String] = None) extends Store[Unit]
  case class Exists(metaId:String, id:String) extends Store[Boolean]
}