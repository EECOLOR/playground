package org.qirx.cms

import machinery.~>
import construction.Store
import scala.concurrent.Future
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.construction.Index

trait Environment {
  def store:Store ~> Future
  def index:Index ~> Future
  def reportDocumentMetadataMismatch(document:JsObject, meta:DocumentMetadata, validationResults:Seq[JsObject]):Unit
}