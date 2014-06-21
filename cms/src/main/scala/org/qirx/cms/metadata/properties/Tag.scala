package org.qirx.cms.metadata.properties

import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.dsl.Property

class Tag(id:String) extends Property(id) with Identifiable
object Tag extends Tag("tag")