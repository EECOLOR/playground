package testUtils

import org.qirx.cms.Environment
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import scala.collection.mutable.ListBuffer

class TestEnvironment extends Environment {
  val store = new TestStore
  val index = new TestIndex
  def reportDocumentMetadataMismatch(document:JsObject, meta:DocumentMetadata, validationResults:Seq[JsObject]):Unit = ???
}