package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

trait Identifiable { self: Property =>
  private def set(nonEmpty: Boolean) = new ValueSetProperty(self, nonEmpty = true)
  //def + = set(nonEmpty = true)
  def * = set(nonEmpty = false)
}

class ValueSetProperty(property: Property with Identifiable, nonEmpty: Boolean)
  extends WrappedProperty(if (nonEmpty) "+" else "*", property)