package org.qirx.cms.metadata.properties

import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.Property

class Date(id:String) extends Property(id) with GeneratableValue
object Date extends Date("date")
