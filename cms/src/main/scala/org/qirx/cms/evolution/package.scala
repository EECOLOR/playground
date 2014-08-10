package org.qirx.cms

import play.api.libs.json.Reads
import play.api.libs.json.JsObject

package object evolution {
  type Transformation = JsObject => JsObject
  type Evolution = (Int, Transformation)
}