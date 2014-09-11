package org.qirx.cms.elasticsearch

import play.api.libs.json.JsObject

trait PropertyIndexInformation {
  def mappings(name:String):Seq[JsObject]
  def transform(name:String, document:JsObject):JsObject
}