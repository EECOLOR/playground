package org.qirx.cms

import scala.concurrent.Future

import org.qirx.cms.construction.Index
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata

import construction.Store
import play.api.libs.json.JsObject

trait Environment {
  def store:Store ~> Future
  def index:Index ~> Future
  def reportDocumentMetadataMismatch(document:JsObject, meta:DocumentMetadata, validationResults:Seq[JsObject]):Unit
}