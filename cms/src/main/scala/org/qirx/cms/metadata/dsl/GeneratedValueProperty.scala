package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

trait GeneratableValue { self: PropertyMetadata =>
  def generated = new GeneratedValueProperty(self)
}

class GeneratedValueProperty(property: PropertyMetadata with GeneratableValue)
  extends WrappedProperty("generated", property)
