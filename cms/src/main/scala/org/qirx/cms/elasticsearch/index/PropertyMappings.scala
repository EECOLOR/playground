package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsObject

trait PropertyMappings {
  def mappings(name:String):Seq[JsObject]
}