package org.qirx.cms

import play.api.libs.json.Reads
import play.api.libs.json.JsObject

package object metadata {
  type Transformation = Reads[JsObject]
  type Evolution = (Int, Transformation)
}