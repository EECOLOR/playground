package org.qirx.cms.metadata

import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import play.api.libs.json.Writes
import scala.language.higherKinds
import org.qirx.cms.testing.TypeclassMagnet

trait PropertyMetadata {
  def id: String
  def confidential:Boolean
  def generator:Option[ValueGenerator]
  
  def validate(messages:Messages, value:Option[JsValue]):Option[JsObject]
  
  def toJson:JsObject
}
