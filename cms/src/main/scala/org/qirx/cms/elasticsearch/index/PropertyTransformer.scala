package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsObject

trait PropertyTransformer {
  def transform(name:String, document:JsObject):JsObject
}