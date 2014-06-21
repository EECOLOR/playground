package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

class Property(val id: String) extends PropertyMetadata {
  def ? = new OptionalValueProperty(this)
}
