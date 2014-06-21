package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

class OptionalValueProperty(property: PropertyMetadata)
  extends WrappedProperty("?", property)