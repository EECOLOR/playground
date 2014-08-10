package org.qirx.cms.metadata

import play.api.libs.json.JsObject

trait DocumentIdGenerator {

  def generateFor(value:JsObject):String
  def makeUnique(id:String):String
}