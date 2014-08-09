package org.qirx.cms.metadata

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages

trait PropertyMetadata {
  def id: String
  def confidential:Boolean
  
  def validate(messages:Messages, value:Option[JsValue]):Option[JsObject]
}