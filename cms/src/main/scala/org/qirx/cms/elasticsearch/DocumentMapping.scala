package org.qirx.cms.elasticsearch

import play.api.libs.json.JsObject

trait DocumentMapping {
  def mapping:JsObject
  def transform(o:JsObject):JsObject
}