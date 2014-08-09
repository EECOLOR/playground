package org.qirx.cms.metadata

import scala.collection.immutable.ListMap

trait DocumentMetadata {
  def id: String
  def idGenerator: DocumentIdGenerator
  def properties: ListMap[String, PropertyMetadata]
  def evolutions:Evolutions
  
  def withEvolutions(evolutions:Evolution*):DocumentMetadata
}