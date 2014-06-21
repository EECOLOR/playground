package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.PropertyMetadata

object Document {
  def apply(id: String)(properties: (String, PropertyMetadata)*): DocumentMetadata = 
    new DefaultDocument(id, properties.toMap)

  private class DefaultDocument(
    val id: String,
    val properties: Map[String, PropertyMetadata]) extends DocumentMetadata
}