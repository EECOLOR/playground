package org.qirx.cms

import machinery.~>
import construction.Store
import scala.concurrent.Future
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata

trait Environment {
  def store:Store ~> Future
  def reportDocumentMetadataMismatch(document:JsObject, meta:DocumentMetadata, validationResults:Seq[JsObject]):Unit
}