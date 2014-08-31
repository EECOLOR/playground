package org.qirx.cms.testing

import play.api.libs.json.JsObject
import scala.concurrent.Future

trait StoreWrapper {
  def list(metaId: String, fieldSet: Set[String] = Set.empty): Future[Seq[JsObject]]
  def get(metaId: String, id: String, fieldSet: Set[String] = Set.empty): Future[Option[JsObject]]
  def exists(metaId: String, id: String): Future[Boolean]
  def save(metaId: String, id: String, document: JsObject): Future[Unit]
  def addId(metaId: String, id: String, newId: String): Future[Unit]
  def delete(metaId: String, id: String): Future[Unit]
  def deleteAll(metaId: String): Future[Unit]
}