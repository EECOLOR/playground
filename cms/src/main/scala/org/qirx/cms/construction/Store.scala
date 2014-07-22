package org.qirx.cms.construction

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata

sealed trait Store[T]
case class List(meta: DocumentMetadata, fieldSet: Set[String]) extends Store[Seq[JsObject]]
case class Get(meta: DocumentMetadata, id: String, fieldSet: Set[String]) extends Store[Option[JsObject]]
case class Create(meta: DocumentMetadata, document: JsObject) extends Store[String]
case class Update(meta: DocumentMetadata, id: String, oldDocument: JsObject, newDocument: JsObject, fieldSet:Set[String]) extends Store[Unit]
