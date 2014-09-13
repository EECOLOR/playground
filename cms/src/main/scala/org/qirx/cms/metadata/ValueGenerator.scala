package org.qirx.cms.metadata

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject

trait ValueGenerator {
  def generate(propertyName:String, document:JsObject):JsValue
}