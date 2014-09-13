package org.qirx.cms.elasticsearch.index

import play.api.libs.json.JsObject

trait DocumentMappings {
  def mapping:JsObject
}
