package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsObject

trait DocumentTransformer {
  def transform(o:JsObject):JsObject
}