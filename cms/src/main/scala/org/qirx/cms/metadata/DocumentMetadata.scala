package org.qirx.cms.metadata

import scala.collection.immutable.ListMap
import org.qirx.cms.evolution.Evolutions
import org.qirx.cms.evolution.Evolution
import play.api.libs.json.JsObject
import play.api.libs.json.Writes
import play.api.libs.json.Json

trait DocumentMetadata {
  /**
   * The identifier for this document type
   */
  def id: String
  
  /**
   * A generator for the id's of the documents of this document type
   */
  def idGenerator: DocumentIdGenerator
  
  /**
   * The properties of this document type
   */
  type Name = String
  def properties: ListMap[Name, PropertyMetadata]
  
  /**
   * The evolutions for the documents of this document type
   */
  def evolutions:Evolutions
  
  /**
   * A utility method to add evolutions
   */
  def withEvolutions(evolutions:Evolution*):DocumentMetadata
  
  /**
   * Converts this document type to a json object
   */
  def toJson:JsObject
}

object DocumentMetadata {
  implicit val writes = Writes[DocumentMetadata](_.toJson)
}