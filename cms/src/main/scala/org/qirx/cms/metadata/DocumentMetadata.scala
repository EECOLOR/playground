package org.qirx.cms.metadata

import play.api.libs.json.JsObject

trait DocumentMetadata {
  def id: String
  def idGenerator: DocumentIdGenerator
  def properties: Map[String, PropertyMetadata]
  def evolutions:Seq[Evolution]
  
  def withEvolutions(evolutions:Evolution*):DocumentMetadata
}