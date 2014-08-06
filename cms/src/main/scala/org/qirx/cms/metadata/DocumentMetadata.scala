package org.qirx.cms.metadata

trait DocumentMetadata {
  def id: String
  def idGenerator: DocumentIdGenerator
  def properties: Map[String, PropertyMetadata]
  def evolutions:Evolutions
  
  def withEvolutions(evolutions:Evolution*):DocumentMetadata
}