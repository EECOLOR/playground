package org.qirx.cms.metadata.dsl

import org.qirx.cms.metadata.PropertyMetadata

abstract class WrappedProperty(property:PropertyMetadata) extends PropertyMetadata {
  final val id = property.id
  final val confidential = property.confidential
}