package org.qirx.cms.construction

import play.api.libs.json.JsObject
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result

sealed trait Index[T]
object Index {
  case class Get(metaId: String, id: String, fieldSet: Set[String]) extends Index[Option[JsObject]]
  case class List(metaId: String, fieldSet: Set[String]) extends Index[Seq[JsObject]]
  case class Put(metaId: String, id: String, document: JsObject) extends Index[Unit]
  case class Delete(metaId: String, id: Option[String] = None) extends Index[Unit]
  case class Search(request:Request[AnyContent], remainingPathSegments:Seq[String]) extends Index[Result]
}