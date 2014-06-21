package org.qirx.cms.metadata

trait DocumentMetadata {
  def id: String
  def properties: Map[String, PropertyMetadata]
}