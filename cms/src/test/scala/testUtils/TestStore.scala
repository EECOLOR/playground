package testUtils

import scala.concurrent.Future
import scala.collection.mutable
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.collection.mutable.ListBuffer
import play.api.libs.json.JsString
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Store._
import org.qirx.cms.testing.MemoryStore

class TestStore extends MemoryStore