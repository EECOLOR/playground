package org.qirx.cms.system

import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentIdGenerator
import org.qirx.cms.metadata.DocumentMetadata

sealed trait Action[T]

case class Get(document:DocumentMetadata, id:String, fieldSet:Set[String]) extends Action[Option[JsObject]]
case class Create(document:DocumentMetadata, obj:JsObject) extends Action[String]
case class Update(document:DocumentMetadata, oldObj:JsObject, newObj:JsObject, fieldSet:Set[String]) extends Action[Unit]
case class List(document:DocumentMetadata, fields:Set[String]) extends Action[Seq[JsObject]]