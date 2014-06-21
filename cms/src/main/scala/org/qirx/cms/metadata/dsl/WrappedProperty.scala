package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

class WrappedProperty(containerId:String, property:PropertyMetadata) extends PropertyMetadata {
  final val id = property.id + "." + containerId
}